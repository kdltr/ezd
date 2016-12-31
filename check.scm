(use scheme2c-compatibility xlib posix srfi-18 srfi-1 srfi-4)

;; System file tasks

(define *tasks* '())
(define *tasks-enabled* #f)

(define (define-system-file-task file idle-task file-task)
  (set! *tasks* (cons (list file idle-task file-task) *tasks*)))

(define (enable-system-file-tasks flag)
  (set! *tasks-enabled* flag))

(define (wait-system-file exp)
  (void))

(define (run-tasks escape-file)
  (let ((ready (file-select (cons escape-file (map car *tasks*)) #f)))
    (if (member escape-file ready)
        #t
        (begin
          (for-each
            (lambda (fd)
              ((caddr (assoc fd *tasks*))))
            ready)
          (run-tasks escape-file)))))

(define (ezd-read)
  (when (and (not (char-ready?)) *tasks-enabled*)
    (let ((idle-tasks (map cadr *tasks*)))
      (for-each (lambda (thunk) (thunk)) idle-tasks)
      (run-tasks (port->fileno (current-input-port)))))
  (read))


(define putprop put!)
(define getprop get)

(define (remq x list)
  (delete x list eq?))

;; TODO test this
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

(include "commands.sc")
(include "ezd.sc")
(include "ginfo.sc")
(include "display.sc")
(include "window.sc")
(include "drawing.sc")
(include "view.sc")
(include "graphic.sc")
(include "rectangle.sc")
(include "events.sc")
(include "interact.sc")

(read-eval-draw (command-line-arguments))