(module ezd-external (ezd ezd-options ezd-output-port ezd-input-port)

(import scheme chicken)
(use posix)

(define ezd-output-port #f) ;; ezd commands are written to this port
(define ezd-input-port #f) ;; ezd events are read from this port

(define ezd-options '())

(define (start-ezd)
  (let-values (((toin toout) (create-pipe))
               ((fromin fromout) (create-pipe)))
    (process-fork (lambda ()
                    (duplicate-fileno toin 0)
                    (file-close toin)
                    (file-close toout)
                    (duplicate-fileno fromout 1)
                    (file-close fromin)
                    (file-close fromout)
                    (process-execute "ezd" ezd-options)
                    (exit 1))
                  #t)
    (file-close toin)
    (set! ezd-output-port (open-output-file* toout))
    (file-close fromout)
    (set! ezd-input-port (open-input-file* fromin))))

(define (ezd . cmds)
  (unless ezd-output-port
    (start-ezd))
  (for-each (lambda (c) (write c ezd-output-port)) cmds)
  (flush-output ezd-output-port))

)
