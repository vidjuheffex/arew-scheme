(import (only (chezscheme)
              file-directory?
              directory-list
              compile-profile
              profile-dump-html
              expand
              annotation?
              pretty-print
              import
              copy-environment
              environment
              eval
              make-source-file-descriptor
              open-source-file
              get-datum/annotations
              open-file-input-port
              annotation-expression
              source-directories
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

(define (eval* obj)
  (let ((env (copy-environment (environment '(only (chezscheme) import)) #t)))
    (let loop ((program (if (pair? obj) obj (read-filename obj))))
      (unless (null? program)
        (eval (car program) env)
        (loop (cdr program))))))

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

(match (cdr (command-line))
;;  (("editor" filename) (editor filename))
  (("eval" filename) (eval* filename))
  (("check" filename) (check filename))
  (else (display "unknown subcommand.\n")))
