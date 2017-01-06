;; System file tasks

(define *tasks* '())
(define *tasks-enabled* #f)

(define (define-system-file-task file idle-task file-task)
  (set! *tasks* (cons (list file idle-task file-task) *tasks*)))

(define (enable-system-file-tasks flag)
  (set! *tasks-enabled* flag))

(define (wait-system-file escape-file)
  (let-values (((ready _) (file-select (if escape-file
                                           (cons escape-file (map car *tasks*))
                                           (map car *tasks*))
                                       #f)))
    (if (member escape-file ready)
        #t
        (begin
          (for-each
            (lambda (fd)
              ((caddr (assoc fd *tasks*))))
            ready)
          (wait-system-file escape-file)))))

(define (ezd-read)
  (when (and (not (char-ready?)) *tasks-enabled*)
    (let ((idle-tasks (map cadr *tasks*)))
      (for-each (lambda (thunk) (thunk)) idle-tasks)
      (wait-system-file (port->fileno (current-input-port)))))
  (read))

(define (ezd-repl)
  (display "#;> ")
  (flush-output)
  (let ((in (ezd-read)))
    (unless (eof-object? in)
      (print (eval in))
      (ezd-repl))))

(define putprop put!)
(define getprop get)

(define (remq x list)
  (delete x list eq?))

(define (xrectangle-list->xrectanglea rl)
  (let ((v (make-u16vector (* 4 (length rl))))
        (n 0))
    (for-each
      (lambda (r)
        (u16vector-set! v (+ n 0) (xrectangle-x r))
        (u16vector-set! v (+ n 1) (xrectangle-y r))
        (u16vector-set! v (+ n 2) (xrectangle-width r))
        (u16vector-set! v (+ n 3) (xrectangle-height r))
        (set! n (+ n 4)))
      rl)
    (if (zero? (u16vector-length v))
        #f
        (make-locative v))))

 
;; Macros that help the Scheme2c to CHICKEN conversion

#;(define string->symbol
  (let ((orig string->symbol))
    (lambda (x)
      (orig (string-downcase x)))))

(define-syntax define-external
  (syntax-rules ()
    ((define-external symbol . rest)
     (declare (export symbol)))))
