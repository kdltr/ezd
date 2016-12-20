;; Macros that help the Scheme2c to CHICKEN conversion

#;(define string->symbol
  (let ((orig string->symbol))
    (lambda (x)
      (orig (string-downcase x)))))

#;(define-syntax define-external
  (syntax-rules ()
    ((define-external symbol . rest)
     (declare (export symbol)))))

(define-syntax define-external
  (syntax-rules ()
    ((define-external . rest)
     (void))))
