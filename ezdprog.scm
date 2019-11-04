(module ezdprog ()

(import scheme (chicken base) (chicken process-context) ezd)

;; make the ezd procedure available in the REDL
(eval '(import ezd))

(read-eval-draw (command-line-arguments)))
