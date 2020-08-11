(library (arew network epoll)
  (export make-epoll
          epoll-watch
          epoll-unwatch
          epoll-generator)

  (import (chezscheme))
  
  (begin

    (define stdlib (load-shared-object #f))

    ;; high level interface

    (define (make-epoll)
      (raise 'not-implemented))

    (define (epoll-watch-all! epfd fd)
      ;; XXX: This assumes the file descriptor is already registred,
      ;; and rely on EPOLL_CTL_MOD
      (define event (make-epoll-events 1))
      (epoll-event-event-set! event (bitwise-ior EPOLLIN EPOLLOUT))
      (epoll-ctl! epfd EPOLL_CTL_MOD fd event)
      (foreign-free (ftype-pointer-address event)))

    (define (epoll-watch-read! epfd fd)
      (define event (make-epoll-events 1))
      (epoll-event-event-set! event (bitwise-ior EPOLLIN))
      (epoll-ctl! epfd EPOLL_CTL_ADD fd event)
      (foreign-free (ftype-pointer-address event)))

    (define (epoll-watch-write! epfd fd)
      (define event (make-epoll-events 1))
      (epoll-event-event-set! event (bitwise-ior EPOLLOUT))
      (epoll-ctl! epfd EPOLL_CTL_ADD fd event)
      (foreign-free (ftype-pointer-address event)))

    (define epoll-watch!
      (case-lambda
        ((epfd fd) (epoll-watch-all! epfd fd))
        ((epfd fd write-or-read)
         (case write-or-read
           ((write) (epoll-watch-read! epfd fd))
           ((read) (epoll-watch-write! epfd fd))
           (else (error 'epoll "unknown watch flag" write-or-read))))))

    (define (epoll-unwatch epfd fd)
      (epoll-ctl! epfd EPOLL_CTL_DEL fd 0))

    (define (epoll-wait* epfd max-events timeout)

      (define events (make-epoll-events max-events))
      (define count (epoll-wait epfd events max-events timeout))

      (define (continue/write fd)
        (set! continue start)
        (cons 'write fd))
      
      (define (continue/both fd)
        (set! continue/write fd)
        (cons 'read fd))
      
      (define (start)
        (if (fx=? count 0)
            (begin
             (foreign-free (ftype-pointer-address events))
             (eof-object))
            (begin
              (set! count (fx- count 1))
              (let* ((events (epoll-event-events-ref events count))
                     (in? (fx=? (bitwise-and events EPOLLIN events) EPOLLIN))
                     (out? (fx=? (bitwise-and events EPOLLOUT events) EPOLLOUT))
                     (fd (epoll-event-fd-ref events count)))
              (cond
               ((and in? out?) (continue/both fd))
               (in? (cons 'write fd))
               (out? (cons 'read fd))
               (else (error 'epoll "Oops!")))))))

      (define continue start)
      
      (lambda ()
        (continue)))


    ;; low level interface

    ;; TODO: add errno to errors
      
    (define EPOLL_CTL_ADD 1)
    (define EPOLL_CTL_DEL 2)
    (define EPOLL_CTL_MOD 3)

    (define EPOLLIN #x01)
    (define EPOLLOUT #x04)

    (define-ftype %epoll-data
      (union (ptr void*)
             (fd int)
             (u32 unsigned-32)
             (u64 unsigned-64)))

    (define-ftype %epoll-event
      (struct (events unsigned-32)
              (data %epoll-data)))

    (define (make-epoll-events count)
      (make-ftype-pointer %epoll-event
                          (foreign-alloc (* count (ftype-sizeof %epoll-event)))))

    (define (epoll-event-fd-ref events index)
      (ftype-ref %epoll-event (data fd) events index))

    (define (epoll-event-events-ref events index)
      (ftype-ref %epoll-event (events) events index))
    
    (define (epoll-event-event-set! event flag)
      (ftype-set! %epoll-event (events) event flag))

    (define (epoll-event-fd-set! event fd)
      (ftype-set! %epoll-event (data fd) event fd))

    (define epoll-create
      (let ((func (foreign-procedure "epoll_create1" (int) int)))
        (lambda ()
          (let ((out (func 1)))
            (if (fx=? out -1)
                (error 'epoll "epoll-create failed" out)
                out)))))

    (define epoll-ctl!
      (let ((func (foreign-procedure "epoll_ctl" (int int int void*) int)))
        (lambda (epoll op fd event)
          (when (fx= (func epoll op fd (ftype-pointer-address event)) -1)
            (error 'epoll "epoll-ctl! failed")))))

    (define epoll-wait
      (let ([func (foreign-procedure "epoll_wait" (int void* int int) int)])
        (lambda (epoll events max-events timeout)
          (let ((out (func epoll (ftype-pointer-address events) max-events timeout)))
            (if (fx=? out -1)
                (error 'epoll "epoll failed" out)
                out)))))))
