(module ezdprog ()

(import scheme chicken)
(use ezd)

;; make the ezd procedure available in the REDL
(eval '(import ezd))

(read-eval-draw (command-line-arguments)))
