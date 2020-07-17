(import (only (chezscheme)
              annotation-expression
              annotation?
              bytevector->u8-list
              compile-program
              compile-whole-program
              compile-imported-libraries
              get-bytevector-all
              compile-profile
              copy-environment
              directory-list
              environment
              eval
              expand
              file-directory?
              format
              generate-wpo-files
              get-datum/annotations
              import
              make-boot-file
              make-source-file-descriptor
              open-file-input-port
              open-source-file
              pretty-print
              profile-dump-html
              source-directories
              system
              with-source-path))

(import (scheme base))
(import (scheme list))
(import (scheme file))
(import (scheme read))
(import (scheme process-context))
(import (scheme write))
(import (scheme hash-table))
(import (scheme comparator))
(import (srfi srfi-145))
(import (arew matchable))
;; (import (arew editor))

;; helpers

(define (topological-sort dependencies)

  (define (remove-self-dependency pair)
    (let ((key (car pair))
          (value (cdr pair)))
      (cons key (delete key value equal?))))

  (define (remove-self-dependencies alist)
    (map remove-self-dependency alist))

  (define (add-missing-items dependencies)
    (let loop ((items (delete-duplicates (append-map cdr dependencies) equal?))
               (out dependencies))
      (if (null? items)
          out
          (let ((item (car items)))
            (if (assoc item out)
                (loop (cdr items) out)
                (loop (cdr items) (cons (cons item '()) out)))))))

  (define (lift dependencies batch)
    (let loop ((dependencies dependencies)
               (out '()))
      (if (null? dependencies)
          out
          (let ((key (caar dependencies))
                (value (cdar dependencies)))
            (if (null? value)
                (loop (cdr dependencies) out)
                (loop (cdr dependencies)
                      (cons (cons key (lset-difference equal? value batch))
                            out)))))))


  (let* ((dependencies (remove-self-dependencies dependencies))
         (dependencies (add-missing-items dependencies)))
    (let loop ((out '())
               (dependencies dependencies))
      (if (null? dependencies)
          (reverse (apply append out))
          (let ((batch (map car (filter (lambda (pair) (null? (cdr pair))) dependencies))))
            (if (null? batch)
                #f
                (loop (cons batch out) (lift dependencies batch))))))))

(define string-join
  (lambda (str* jstr)
    (cond
     [(null? str*) ""]
     [(null? (cdr str*)) (car str*)]
     [else (string-append (car str*) jstr (string-join (cdr str*) jstr))])))


(define %arew-path
  (get-environment-variable "AREW_PATH"))

(source-directories (list %arew-path "."))

(define find-file
  (lambda (fn)
    (with-source-path 'find-file fn values)))

(define %arew-library-extensions
  '(".arew.sld" ".arew.sls" ".arew.scm"
    ".chezscheme.sld" ".chezscheme.sls" ".chezscheme.scm" ".chezscheme.ss"
    ".sld" ".sls" ".scm" ".ss"))

(define (annotations->datum obj)
  (cond
   ((pair? obj) (cons (annotations->datum (car obj))
                      (annotations->datum (cdr obj))))
   ((vector? obj) (vector-map annotations->datum obj))
   ((annotation? obj) (annotations->datum (annotation-expression obj)))
   (else obj)))

(define (primitive-import? name)
  (case (car name)
    ((chezscheme rnrs) #t)
    (else #f)))

(define (pack filename-or-obj)

  (define (maybe-annotation-expression obj)
    (if (annotation? obj)
        (annotation-expression obj)
        obj))

  (define (and=> v proc)
    (if v (proc v) #f))

  (define unique-var
    (let ((count 0))
      (lambda (name)
        (let ([count* count])
          (set! count (+ count 1))
          (string->symbol
           (string-append (symbol->string name) "." (number->string count*)))))))

  (define (memoize proc)
    (let ((memory '()))
      (lambda (name)
        (or (and=> (assoc name memory) cdr)
            (let ((new-name (proc name)))
              (set! memory (cons (cons name new-name)
                                 memory))
              new-name)))))

  (define rename
    (memoize
     (lambda (name)
       (case (car name)
         ((chezscheme rnrs) name)
         (else (list (unique-var 'library)))))))

  (define (import-rename spec)
    (let ((spec (maybe-annotation-expression spec)))
      (case (maybe-annotation-expression (car spec))
        ((prefix rename for except only)
         (cons* (car spec) (import-rename (cadr spec)) (cddr spec)))
        (else (rename (map maybe-annotation-expression spec))))))

  (define (read-filename filename)
    (define port (open-file-input-port filename))
    (define sfd (make-source-file-descriptor filename port))
    (define source (open-source-file sfd))
    (let loop ((out '()))
      (call-with-values (lambda () (get-datum/annotations source sfd 0))
        (lambda (object _)
          (if (eof-object? object)
              (reverse out)
              (loop (cons object out)))))))

  (define (library-filepath name)
    (let ((name* (string-join (map symbol->string name) "/")))
      (let loop ((extensions %arew-library-extensions))
        (if (null? extensions)
            (error "Library not found" name)
            (guard (ex (else (loop (cdr extensions))))
              (find-file (string-append name* (car extensions))))))))

  (define (read-program sexp)
    (let loop ((imports '())
               (sexp sexp))
      (if (null? sexp)
          (values '() imports)
          (let ((head (car sexp)))
            (if (and (pair? (maybe-annotation-expression head))
                     (eq? (maybe-annotation-expression (car (maybe-annotation-expression head)))
                          'import))
                (loop (append (cdr (maybe-annotation-expression head)) imports) (cdr sexp))
                (values (reverse imports) sexp))))))

  (define (read-library name)
    (let ((sexp (car (read-filename (library-filepath name)))))
      (let loop ((body (cddr (maybe-annotation-expression sexp)))
                 (exports '())
                 (imports '()))
        (if (null? body)
            (values (reverse exports) (reverse imports) '())
            (let ((head (maybe-annotation-expression (car body))))
              (case (maybe-annotation-expression (car head))
                ((export) (loop (cdr body) (append (cdr head) exports) imports))
                ((import) (loop (cdr body) exports (append (cdr head) imports)))
                (else (values (reverse exports) (reverse imports) body))))))))

  (define (import->name spec)
    (let ((spec (annotations->datum spec)))
      (case (car spec)
        ((rename only prefix except for) (import->name (cadr spec)))
        (else spec))))

  (define (library-imports name)
    (if (primitive-import? name)
        '()
        (call-with-values (lambda () (read-library name))
          (lambda (exports imports body)
            (map import->name imports)))))

  (define (make-dependencies imports)
    (let loop ((imports imports)
               (out '()))
      (if (null? imports)
          out
          (if (assoc (car imports) out)
              (loop (cdr imports) out)
              (let ((imports* (library-imports (car imports))))
                (loop (append (cdr imports) imports*)
                      (cons (cons (car imports) imports*)
                            out)))))))

  (define (libraries-pack name)
    (call-with-values (lambda () (read-library name))
      (lambda (exports imports body)
        `($library ,(rename name)
                   (export ,@exports)
                   (import ,@(map import-rename imports))
                   ,@body))))


  (define (maybe-read-program filename-or-obj)
    (if (string? filename-or-obj)
        (read-program (read-filename filename-or-obj))
        (read-program filename-or-obj)))

  (call-with-values (lambda () (maybe-read-program filename-or-obj))
    (lambda (imports body)
      (when (null? imports)
        (error "No imports" filename-or-obj))
      (when (null? body)
        (error "No expression" filename-or-obj))

      (let* ((imports* (map import->name imports))
             (dependencies (make-dependencies imports*))
             (libraries (remove primitive-import? (topological-sort dependencies))))

        `($begin
          ,@(map libraries-pack libraries)
          ($import ,@(map import-rename imports))
          ,@body)))))

(define (print filename)
  (pretty-print (annotations->datum (pack filename))))

(define (eval* obj)
  (let ((env (copy-environment (environment '(prefix (arew r7rs) $)))))
    (eval (pack obj) env)))

(define (expand* filename)
  (let ((env (copy-environment (environment '(prefix (arew r7rs) $)))))
    (pretty-print (expand (pack filename) env))))

(define (check path)

  (define (string-suffix? string suffix)
    (let loop ((path (reverse (string->list string)))
               (suffix (reverse (string->list suffix))))
      (if (null? suffix)
          #t
          (if (char=? (car path) (car suffix))
              (loop (cdr path) (cdr suffix))
              #f))))

  (define (path-join path item)
    (if (string-suffix? path "/")
        (string-append path item)
        (string-append path "/" item)))

  (define (discover path)
    (let loop ((paths (list path))
               (out '()))
      (if (null? paths)
          out
          (let ((path (car paths)))
            (if (file-directory? path)
                (loop (append (cdr paths)
                              (map (lambda (item) (path-join path item))
                                   (directory-list path)))
                      out)
                (if (string-suffix? path "check.scm")
                    (loop (cdr paths) (cons path out))
                    (loop (cdr paths) out)))))))

  (define (read-filename filename)
    (define port (open-file-input-port filename))
    (define sfd (make-source-file-descriptor filename port))
    (define source (open-source-file sfd))
    (let loop ((out '()))
      (call-with-values (lambda () (get-datum/annotations source sfd 0))
        (lambda (object _)
          (if (eof-object? object)
              (reverse out)
              (loop (cons object out)))))))

  (define (library-filepath name)
    (let ((name* (string-join (map symbol->string name) "/")))
      (let loop ((extensions %arew-library-extensions))
        (if (null? extensions)
            (error "Library not found" name)
            (guard (ex (else (loop (cdr extensions))))
              (find-file (string-append name* (car extensions))))))))

  (define (read-library name)
    (let ((sexp (car (read-filename (library-filepath name)))))
      (let loop ((body (cddr (annotation-expression sexp)))
                 (exports '())
                 (imports '()))
        (if (null? body)
            (values (reverse exports) (reverse imports) '())
            (let ((head (annotation-expression (car body))))
              (case (annotation-expression (car head))
                ((export) (loop (cdr body) (append (cdr head) exports) imports))
                ((import) (loop (cdr body) exports (append (cdr head) imports)))
                (else (values exports imports body))))))))

  (define (path->library-name path)
    (cadr (annotations->datum (car (read-filename path)))))

  (define (import->name spec)
    (let ((spec (annotations->datum spec)))
      (case (car spec)
        ((rename only prefix except for) (import->name (cadr spec)))
        (else spec))))

  (define (library-imports name)
    (if (primitive-import? name)
        '()
        (call-with-values (lambda () (read-library name))
          (lambda (exports imports body)
            (map import->name imports)))))

  (define (library-exports name)
    (if (primitive-import? name)
        '()
        (call-with-values (lambda () (read-library name))
          (lambda (exports imports body) (map annotation-expression exports)))))

  (define (make-dependencies imports)
    (let loop ((imports imports)
               (out '()))
      (if (null? imports)
          out
          (if (assoc (car imports) out)
              (loop (cdr imports) out)
              (let ((imports* (library-imports (car imports))))
                (loop (append (cdr imports) imports*)
                      (cons (cons (car imports) imports*)
                            out)))))))

  (define (make-check-program libraries)

    (define (make-library/prefix name index)
      (cons name (string->symbol (string-append "checks-" (number->string index) ":"))))

    (define libraries/prefix (map make-library/prefix libraries (iota (length libraries))))

    (define (symbol-append a b)
      (string->symbol (string-append (symbol->string a) (symbol->string b))))

    (define (magic library/prefix)
      (let ((checks (library-exports (car library/prefix))))
        (map (lambda (check) `(run-check ',(car library/prefix)
                                         ',check
                                         ,(symbol-append (cdr library/prefix) check)))
             checks)))

    `((import (check))
      ,@(map (lambda (library/prefix) `(import (prefix ,(car library/prefix) ,(cdr library/prefix)))) libraries/prefix)

      ,@(append-map magic libraries/prefix)))

  (define names (map path->library-name (discover path)))

  (define dependencies (make-dependencies names))

  (define sorted (topological-sort dependencies))

  (define sorted-check-libraries (filter (lambda (x) (find (lambda (y) (equal? x y)) names))
                                          sorted))

  (define sorted-check-libraries* (remove (lambda (x) (equal? x '(check)))
                                          sorted-check-libraries))

  (define program (make-check-program sorted-check-libraries*))

  (unless (null? sorted-check-libraries*)
    (parameterize ([compile-profile 'source])
      (eval* program)
      (profile-dump-html "profile/"))))

(define (compile filename)

  (define stubs "int setupterm(char *term, int fd, int *errret) {
	return 0;
}

int tputs(const char *str, int affcnt, int (*putc)(int)) {
	return 0;
}

void *cur_term;")

  (define embed
    "#include <assert.h>
#include <fcntl.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <scheme.h>

extern const char chezschemebootfile;
extern const unsigned chezschemebootfile_size;
extern const char scheme_program;
extern const unsigned scheme_program_size;

extern int run_program(int argc, const char **argv, const char *bootfilename, const char *schemefilename);

char bootfilename[] = \"/tmp/chezschemebootXXXXXX\";
char schemefilename[] = \"/tmp/schemeprogramXXXXXX\";
const char *cleanup_bootfile = 0;
const char *cleanup_schemefile = 0;

void cleanup(void) {
	if (cleanup_bootfile) unlink(bootfilename);
	if (cleanup_schemefile) unlink(schemefilename);
}

int maketempfile(char *template, const char *contents, size_t size) {
	int fd;
	fd = mkstemp(template);
	assert(fd >= 0);

	assert(write(fd, contents, size) == size);
	assert(lseek(fd, 0, SEEK_SET) == 0);
	return fd;
}

int main(int argc, const char **argv) {
	int bootfd;
	int schemefd;
	int ret;

	atexit(cleanup);

	bootfd = maketempfile(bootfilename, &chezschemebootfile, chezschemebootfile_size);
	cleanup_bootfile = bootfilename;
	schemefd = maketempfile(schemefilename, &scheme_program, scheme_program_size);
	cleanup_schemefile = schemefilename;

	ret = run_program(argc, argv, bootfilename, schemefilename);

	close(bootfd);
	close(schemefd);

	return ret;
}")

  (define setup "
#include <scheme.h>

extern const char chezschemebootfile;
extern const unsigned chezschemebootfile_size;
extern const char scheme_program;
extern const unsigned scheme_program_size;

extern int run_program(int argc, const char **argv, const char *bootfilename, const char *schemefilename);

static const char *argv0;

const char *program_name(void) {
	return argv0;
}

void custom_init(void) {
	Sregister_symbol(\"program_name\", (void*)program_name);
}

int run_program(int argc, const char **argv, const char *bootfilename, const char *schemefilename) {
	argv0 = argv[0];
	Sscheme_init(0);
	Sregister_boot_file(bootfilename);
	Sbuild_heap(0, custom_init);
	return Sscheme_program(schemefilename, argc, argv);
}
")

  (define base-boot
    '(let ([program-name
            (foreign-procedure "program_name" () string)])

       (scheme-program
        (lambda (fn . fns)
          (command-line (cons (program-name) fns))
          (command-line-arguments fns)
          (load-program fn)))))

  (define (build-included-binary-file input output symbol-name)
    (call-with-output-file output
      (lambda (port)
        (let ([data (bytevector->u8-list
                     (get-bytevector-all (open-file-input-port input)))])
          (format port "#include <stdint.h>~n")
          (format port "const uint8_t ~a[] = {~{0x~x,~}};~n" symbol-name data)
          (format port "const unsigned int ~a_size = sizeof(~a);~n" symbol-name symbol-name)))))

  (define (expand* obj)
    (let ((env (copy-environment (environment '(prefix (arew r7rs) $)))))
      (expand obj env)))

  ;; shared

  (call-with-output-file "/tmp/test/stubs.c"
    (lambda (port)
      (display stubs port)))

  (call-with-output-file "/tmp/test/embed.c"
    (lambda (port)
      (display embed port)))

  (call-with-output-file "/tmp/test/setup.c"
    (lambda (port)
      (display setup port)))

  (call-with-output-file "/tmp/test/base-boot.scm"
    (lambda (port)
      (write base-boot port)))

  (system "cc -c -o /tmp/test/stubs.o /tmp/test/stubs.c")

  (system "cc -c -o /tmp/test/embed.o /tmp/test/embed.c -I/usr/lib/csv9.5.3/ta6le/ -m64")

  (system "cc -c -o /tmp/test/setup.o /tmp/test/setup.c -I/usr/lib/csv9.5.3/ta6le/ -m64")

  (make-boot-file "/tmp/test/program.boot"
                  '()
                  "/usr/lib/csv9.5.3/ta6le/scheme.boot"
                  "/tmp/test/base-boot.scm")

  (call-with-output-file "/tmp/test/program.c"
    (lambda (port)
      (let ((data (bytevector->u8-list
                   (get-bytevector-all (open-file-input-port "/tmp/test/program.boot")))))
        (format port "#include <stdint.h>~n")
        (format port "const uint8_t ~a[] = {~{0x~x,~}};~n" "chezschemebootfile" data)
        (format port "const unsigned int ~a_size = sizeof(~a);~n" "chezschemebootfile" "chezschemebootfile"))))

  (system "cc -c -o /tmp/test/program.o /tmp/test/program.c -m64")

  (system "ar rcs /tmp/test/boot.a /tmp/test/embed.o /tmp/test/setup.o /tmp/test/stubs.o /tmp/test/program.o /usr/lib/csv9.5.3/ta6le//kernel.o")

  ;; specific

  (call-with-output-file "/tmp/test/program.scm"
    (lambda (port)
      (display "#!chezscheme\n" port)
      (display "(import (prefix (only (chezscheme) begin library import) $))\n" port)
      (pretty-print (annotations->datum (pack filename)) port)))

  (compile-imported-libraries #t)
  (generate-wpo-files #t)

  (pk 'compile)
  (compile-program "/tmp/test/program.scm")
  (pk 'wpo)
  (compile-whole-program "/tmp/test/program.wpo" "/tmp/test/program.chez" #t)
  (pk 'test)
  (build-included-binary-file "/tmp/test/program.chez" "/tmp/test/program.chez.c" "scheme_program")
  (pk 'eventually)
  (system "cc -o example /tmp/test/boot.a /tmp/test/program.chez.c -m64 -ldl -lm -luuid -lpthread")
  )

(match (cdr (command-line))
;;  (("editor" filename) (editor filename))
  (("eval" filename) (eval* filename))
  (("expand" filename) (expand* filename))
  (("print" filename) (print filename))
  (("check" filename) (check filename))
  (("compile" filename) (compile filename))
  (else (display "unknown subcommand.\n")))
