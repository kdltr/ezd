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
  (if (char-whitespace? (peek-char))
      (begin (read-char) (ezd-read))
      (read)))

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

(define (yselect dpy sec nsec)
  (let ((dpy-fd (xconnectionnumber dpy))
        (timeout (+ sec (/ nsec (expt 10 9)))))
    (if (not (zero? (xpending dpy)))
        dpy
        (file-select (list dpy-fd) '() timeout))))
 
;; Macros that help the Scheme2c to CHICKEN conversion

#;(define-syntax define-external
  (syntax-rules ()
    ((define-external symbol . rest)
     (export symbol))))

(define-syntax define-external
  (er-macro-transformer
    (lambda (exp r c)
      `(export ,(string->symbol (string-downcase (symbol->string (cadr exp))))))))


;; This macro is often used in .sch files to expose symbols for structure access.
;; That’s why we redefine it so that it exports structure access symbols from modules.

(define-syntax define-in-line-structure-access
  (er-macro-transformer
    (lambda (exp rename compare?)
      (define struct-name (string->symbol (string-downcase (symbol->string (cadr exp)))))
      (define (make-sym . x)
        (string->symbol (apply string-append
                               (map (lambda (x)
                                      (if (symbol? x)
                                          (symbol->string x)
                                          x))
                                 x))))
      (define (make-isa)
        (make-sym "isa-" struct-name "?"))
      (define (make-load slot)
        (make-sym struct-name "-" slot))
      (define (make-store slot)
        (make-sym struct-name "-" slot "!"))

      `(export
             ,(make-isa)
             ,@(append-map
               (lambda (slot) (list (make-load slot) (make-store slot)))
               (filter identity (cddr exp)))))))


;;; helper macro for temporary binding + free

(define-syntax let-temporary
  (syntax-rules ()
    ((_ ((var val free) ...) body ...)
     (let ((var val) ...)
       (receive results (begin body ...)
         (begin (free var) ...)
         (apply values results))))))
