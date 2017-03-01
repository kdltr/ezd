(module ezd-external (ezd ezd-options ezd-output-port ezd-input-port)

(import scheme chicken)
(use posix)

(define ezd-output-port #f) ;; ezd commands are written to this port
(define ezd-input-port #f) ;; ezd events are read from this port

(define ezd-options '())

(define (start-ezd)
  (let-values (((in out pid) (process "ezd" ezd-options)))
    (set! ezd-input-port in)
    (set! ezd-output-port out)))

(define (ezd . cmds)
  (unless ezd-output-port
    (start-ezd))
  (for-each (lambda (c) (write c ezd-output-port)) cmds)
  (flush-output ezd-output-port))

)
