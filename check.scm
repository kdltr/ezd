(use scheme2c-compatibility xlib posix srfi-18)

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

(include "commands.sc")
(include "ezd.sc")
(include "ginfo.sc")
(include "display.sc")
(include "window.sc")
(include "drawing.sc")
(include "view.sc")
(include "graphic.sc")
(include "events.sc")
(include "rectangle.sc")

(read-eval-draw (command-line-arguments))