(module ezd-external (ezd read-event ezd-options)

(import scheme chicken)
(use posix)

(define out #f) ;; ezd commands are written to this port
(define in #f) ;; ezd events are read from this port

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
    (set! out (open-output-file* toout))
    (file-close fromout)
    (set! in (open-input-file* fromin))))

(define (ezd . cmds)
  (unless out
    (start-ezd))
  (for-each (lambda (c) (write c out)) cmds)
  (flush-output out))

(define (read-event)
  (read in))

)
