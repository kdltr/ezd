;;; A simple postfix calculator based on ezd.  The keyboard looks something
;;; like the following:
;;;
;;;     [                       ]
;;;	[			]
;;;	[			]
;;;	+	D	E	F
;;;	-	A	B	C
;;;	*	7	8	9
;;;	/	4	5	6
;;;	+/-	1	2	3
;;;	ODH	0	ENTER	CLR
;;;	DUP	XCH	ROT	QUIT

;*              Copyright 1990 Digital Equipment Corporation
;*                         All Rights Reserved
;*
;* Permission to use, copy, and modify this software and its documentation is
;* hereby granted only under the following terms and conditions.  Both the
;* above copyright notice and this permission notice must appear in all copies
;* of the software, derivative works or modified versions, and any portions
;* thereof, and both notices must appear in supporting documentation.
;*
;* Users of this software agree to the terms and conditions set forth herein,
;* and hereby grant back to Digital a non-exclusive, unrestricted, royalty-free
;* right and license under any changes, enhancements or extensions made to the
;* core functions of the software, including but not limited to those affording
;* compatibility with other hardware or software environments, but excluding
;* applications which incorporate this software.  Users further agree to use
;* their best efforts to return to Digital any such changes, enhancements or
;* extensions that they make and inform Digital of noteworthy uses of this
;* software.  Correspondence should be provided to Digital at:
;* 
;*                       Director of Licensing
;*                       Western Research Laboratory
;*                       Digital Equipment Corporation
;*                       250 University Avenue
;*                       Palo Alto, California  94301  
;* 
;* This software may be distributed (but not offered for sale or transferred
;* for compensation) to third parties, provided such third parties agree to
;* abide by the terms and conditions of this notice.  
;* 
;* THE SOFTWARE IS PROVIDED "AS IS" AND DIGITAL EQUIPMENT CORP. DISCLAIMS ALL
;* WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
;* MERCHANTABILITY AND FITNESS.   IN NO EVENT SHALL DIGITAL EQUIPMENT
;* CORPORATION BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
;* DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
;* PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
;* ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
;* SOFTWARE.

;;; To run this program:
;;;
;;;	csi -s xc.scm

(use ezd)

(define (start-xc clargs)
    (case (length clargs)
	  ((1) (xc)
	       (ezd '(pause)))
	  ((2) (xc (read (open-input-string (cadr clargs))))
	       (ezd '(pause)))
	  (else (format stderr-port "xc [key-color]~%"))))

(define x-scale 50)
(define y-scale 30)
(define xy-pad 2)
(define key-color 'lightblue)
(define text-color 'black)
(define register-height 18)

(define (xc . x)
    (if (pair? x) (set! key-color (car x)))
    (ezd `(window xc 100 100 ,(* x-scale 4) ,(* y-scale 9) fixed-size))
    (ezd '(set-drawing xc))
    (ezd '(overlay xc xc))

    (make-key 0 2 "+" plus)
    (make-key 1 2 "D" (lambda () (digit-in 13)))
    (make-key 2 2 "E" (lambda () (digit-in 14)))
    (make-key 3 2 "F" (lambda () (digit-in 15)))

    (make-key 0 3 "-" minus)
    (make-key 1 3 "A" (lambda () (digit-in 10)))
    (make-key 2 3 "B" (lambda () (digit-in 11)))
    (make-key 3 3 "C" (lambda () (digit-in 12)))

    (make-key 0 4 "*" times)
    (make-key 1 4 "7" (lambda () (digit-in 7)))
    (make-key 2 4 "8" (lambda () (digit-in 8)))
    (make-key 3 4 "9" (lambda () (digit-in 9)))

    (make-key 0 5 "/" divide)
    (make-key 1 5 "4" (lambda () (digit-in 4)))
    (make-key 2 5 "5" (lambda () (digit-in 5)))
    (make-key 3 5 "6" (lambda () (digit-in 6)))

    (make-key 0 6 "+/-" complement)
    (make-key 1 6 "1" (lambda () (digit-in 1)))
    (make-key 2 6 "2" (lambda () (digit-in 2)))
    (make-key 3 6 "3" (lambda () (digit-in 3)))

    (make-key 0 7 (lambda ()
			  (case number-base
				((8) "OCT")
				((10) "DEC")
				((16) "HEX")))
	 base)
    (make-key 1 7 "0" (lambda () (digit-in 0)))
    (make-key 2 7 "ENTER" enter)
    (make-key 3 7 "POP" pop)

    (make-key 0 8 "DUP" dup)
    (make-key 1 8 "XCH" xch)
    (make-key 2 8 "ROT" rot)
    (make-key 3 8 "QUIT" quit)

    (set-xc-value #f #f #f '()))

;;; Key construction.
		  
(define (make-key x y key action)
    
    (define xx (+ (* x x-scale) xy-pad))
    (define yy (+ (* y y-scale) xy-pad))
    (define width (- x-scale xy-pad xy-pad))
    (define height (- y-scale xy-pad xy-pad))
    
    (define keyname (string->symbol (format "KEY~s~s" x y)))
    
    (define bold #f)
    (define highlight #f)
    
    (define (enter) (if (not *mouse-button1*) (draw-key #t #f)))
    
    (define (exit) (draw-key #f #f))
    
    (define (button-down) (draw-key #t #t))
    
    (define (button-up)
	    (if highlight (action))
	    (draw-key #t #f))
    
    (define (draw-key b h)
	    (set! bold b)
	    (set! highlight h)
	    (ezd '(set-drawing xc))
	    (ezd `(object ,keyname
			  (fill-rectangle ,xx ,yy ,width ,height
			      ,(if h text-color key-color))
			  (rectangle ,xx ,yy ,width ,height)
			  (text ,xx ,yy ,width ,height
				center center ,(if (string? key) key (key))
				,(if h key-color text-color)
				,(if b "8x13bold" "8x13")))))
    
    (draw-key #f #f)
    (ezd `(when ,keyname enter ,enter))
    (ezd `(when ,keyname exit ,exit))
    (ezd `(when ,keyname button1down ,button-down))
    (ezd `(when ,keyname button1up ,button-up)))

;;; Memory.

(define reg-a #f)
(define reg-b #f)
(define reg-c #f)
(define reg-rest '())
(define push-digit #t)

(define (set-xc-value a b c rest)
    (define (register reg reg-value)
	    `(text ,(+ xy-pad xy-pad)
		   ,(+ xy-pad xy-pad (* reg register-height))
		   ,(- (* 4 x-scale) (* 8 xy-pad))
		   ,(- register-height xy-pad) right center
		   ,(if reg-value
			(number->string reg-value number-base)
			"")
		   ,text-color "8x13"))
    (if (eq? a 'pop)
	(begin (set! reg-a b)
	       (set! reg-b c)
	       (if (null? reg-rest)
		   (set! reg-c #f)
		   (begin (set! reg-c (car reg-rest))
			  (set! reg-rest (cdr reg-rest)))))
	(begin (set! reg-a a)
	       (set! reg-b b)
	       (set! reg-c c)
	       (set! reg-rest rest)))
    (ezd '(set-drawing xc))
    (ezd `(object xcvalue
		  (rectangle ,xy-pad ,xy-pad
		      ,(- (* 4 x-scale) xy-pad xy-pad)
		      ,(- (* 2 y-scale) xy-pad xy-pad)
		      ,key-color)
		  ,(register 2 reg-a)
		  ,(register 1 reg-b)
		  ,(register 0 reg-c)))
    (set! push-digit #t))

;;; Key definitions.

(define (digit-in v)
    (when (< v number-base)
	  (if push-digit
	      (set-xc-value v reg-a reg-b (cons reg-c reg-rest))
	      (if (number? reg-a)
		  (if (positive? reg-a)
		      (set-xc-value (+ v (* reg-a number-base)) reg-b reg-c
			  reg-rest)
		      (set-xc-value (- v (* reg-a number-base)) reg-b reg-c
			  reg-rest))
		  (set-xc-value v reg-b reg-c reg-rest)))
	  (set! push-digit #f)))
 
(define (plus)
    (if (and (number? reg-a) (number? reg-b))
	(set-xc-value 'pop (+ reg-a reg-b) reg-c reg-rest)))

(define (minus)
    (if (and (number? reg-a) (number? reg-b))
	(set-xc-value 'pop (- reg-b reg-a) reg-c reg-rest)))

(define (times)
    (if (and (number? reg-a) (number? reg-b))
	(set-xc-value 'pop (* reg-a reg-b) reg-c reg-rest)))

(define (divide)
    (if (and (number? reg-a) (number? reg-b) (not (zero? reg-a)))
	(set-xc-value 'pop (quotient reg-b reg-a) reg-c reg-rest)))

(define (complement)
    (if (and (number? reg-a)) (set-xc-value (- reg-a) reg-b reg-c reg-rest)))

(define number-base 10)

(define (base)
    (set! number-base (cdr (assq number-base '((10 . 16) (16 . 8) (8 . 10)))))
    (set-xc-value reg-a reg-b reg-c reg-rest)
    (set! push-digit #f))

(define (pop) (set-xc-value 'pop reg-b reg-c reg-rest))

(define (enter) (set! push-digit #t))

(define (dup)
    (if (number? reg-a)
	(set-xc-value reg-a reg-a reg-b (cons reg-c reg-rest))))

(define (xch)
    (if (and (number? reg-a) (number? reg-b))
	(set-xc-value reg-b reg-a reg-c reg-rest)))

(define (rot)
    (if (and (number? reg-a) (number? reg-b) (number? reg-c))
	(set-xc-value reg-c reg-a reg-b reg-rest)))

(define (quit) (ezd '(quit)))

(xc)
(ezd '(pause))