;;; ezd - easy drawing for X11.
;;;
;;; Color, Stipple, Font, and Cursor information.

;*           Copyright 1990-1993 Digital Equipment Corporation
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

;;; Set the ISA-COLOR property to the RGB values for X colors as they are
;;; found in the input stream.  N.B.  Color names are those recognized by
;;; the X server.

(define (color? x)
    (and (symbol? x)
	 (or (eq? x 'clear)
	     (getprop x 'isa-color)
	     (let-temporary ((c (make-xcolor) free-xcolor))
                (let ((status (xparsecolor *dpy*
                                         (display-colormap *display*)
                                         (symbol->string x)
                                         c)))
		   (if (zero? status)
		       #f
		       (begin (putprop x 'isa-color
				  (list (quotient (xcolor-red c) 256)
					(quotient (xcolor-green c) 256)
					(quotient (xcolor-blue c) 256)))
			      #t)))))))

;;; A color value is either a color name other than clear or an RGB value.

(define (color-value? x)
    (and (not (eq? x 'clear)) (or (color? x) (and (exact? x) (>= x 0)))))

(define (new-color? x) (and (symbol? x) (not (color? x))))

(define (variable-color? x) (getprop x 'variable-color))

;;; Converts RGB color values to HSV values.

(define (convert-rgb->hsv r g b)
    (let* ((r (/ r 255))
	   (g (/ g 255))
	   (b (/ b 255))
	   (maxrgb (max r g b))
	   (minrgb (min r g b))
	   (delta (- maxrgb minrgb))
	   (value maxrgb)
	   (saturation (if (positive? maxrgb)
			   (/ delta maxrgb)
			   0))
	   (hue 0))
	  (if (not (zero? saturation))
	      (let ((rc (/ (- maxrgb r) delta))
		    (gc (/ (- maxrgb g) delta))
		    (bc (/ (- maxrgb b) delta)))
		   (cond ((= r maxrgb)
			  (set! hue (- bc gc)))
			 ((= g maxrgb)
			  (set! hue (+ 2 (- rc bc))))
			 (else (set! hue (+ 4 (- gc rc)))))
		   (set! hue (* hue 60))
		   (if (negative? hue)
		       (set! hue (+ hue 360)))))
	  (list hue saturation value)))

;;; Converts HSV color values to RGB values.

(define (convert-hsv->rgb h s v)
    (let ((scaled-v (inexact->exact (* v 255))))
	 (if (zero? s)
	     (list scaled-v scaled-v scaled-v)
	     (let* ((h (/ (if (= h 360) 0 h) 60))
		    (i (inexact->exact (floor h)))
		    (f (- h i))
		    (p (inexact->exact (* 255 v (- 1 s))))
		    (q (inexact->exact (* 255 v (- 1 (* s f)))))
		    (t (inexact->exact (* 255 v (- 1 (* s (- 1 f)))))))
		   (case i
			 ((0) (list scaled-v t p))
			 ((1) (list q scaled-v p))
			 ((2) (list p scaled-v t))
			 ((3) (list p q scaled-v))
			 ((4) (list t p scaled-v))
			 ((5) (list scaled-v p q))))))) 

;;; Users can define their own colors using the DEFINE-COLOR command.

(define (command-color-value value)
    (if (pair? value)
	(let ((v 0))
	     (for-each
		 (lambda (x) (set! v (+ (* v 256) x)))
		 (apply convert-hsv->rgb value))
	     v)
	value))

(define-ezd-command
    `(define-color ,new-color?
	 (or (,number? ,number? ,number?) (,color-value?)))
    "(define-color color-name { #xRRGGBB | color-name | H S V })"
    (lambda (name value)
	    (display-define-color *display* name (command-color-value value))))

;;; Users can define modifiable colors by DEFINE-VARIABLE-COLOR.

(define-ezd-command
    `(define-variable-color ,new-color?
	 (or (,number? ,number? ,number?) (,color-value?)))
    "(define-variable-color color-name { #xRRGGBB | color-name | H S V })"
    (lambda (name value)
	    (display-define-variable-color *display* name
		(command-color-value value))))

;;; Users can set the value of a variable color by SET-VARIABLE-COLOR.

(define-ezd-command
    `(set-variable-color ,variable-color?
	 (or (,number? ,number? ,number?) (,color-value?)))
    "(set-variable-color color-name { #xRRGGBB | color-name | H S V })"
    (lambda (name value)
	    (display-set-variable-color *display* name
		(command-color-value value))))

;;; Color values are returned in a message on stdout by the following
;;; command.

(define-ezd-command
    `(get-color-value ,color?)
    "(get-color-value color-name)"
    (lambda (name)
	    (let ((rgb (color? name)))
		 (write `(color-value * * * 0 0 0 0 ,name ,@rgb
			     ,@(apply convert-rgb->hsv rgb)) (current-output-port))
		 (newline (current-output-port)))))

;;; Color values are returned by the following Scheme procedures.

(define (get-hsv-color-value name)
    (and (color? name) (apply convert-rgb->hsv (color? name))))

(define (get-rgb-color-value name)
    (color? name))

;;; A stipple is defined by the following procedure that takes a stipple name
;;; and a list of 4, 8, or 16 bit row values.  The bit values are saved on
;;; the ISA-STIPPLE property of the name.

(define (define-stipple name rows)
    (define (two-wide x) (bit-or (bit-lsh x 8) x))
    (define (four-wide x)
	    (bit-or (bit-lsh x 12) (bit-lsh x 8) (bit-lsh x 4) x))
    (case (length rows)
	  ((4)
	   (putprop name 'isa-stipple
	       (let ((r0 (four-wide (car rows)))
		     (r1 (four-wide (cadr rows)))
		     (r2 (four-wide (caddr rows)))
		     (r3 (four-wide (cadddr rows))))
		    (list r0 r1 r2 r3 r0 r1 r2 r3 r0 r1 r2 r3 r0 r1 r2 r3))))
	  ((8)
	   (putprop name 'isa-stipple
	       (let ((r0 (two-wide (list-ref rows 0)))
		     (r1 (two-wide (list-ref rows 1)))
		     (r2 (two-wide (list-ref rows 2)))
		     (r3 (two-wide (list-ref rows 3)))
		     (r4 (two-wide (list-ref rows 4)))
		     (r5 (two-wide (list-ref rows 5)))
		     (r6 (two-wide (list-ref rows 6)))
		     (r7 (two-wide (list-ref rows 7))))
		    (list r0 r1 r2 r3 r4 r5 r6 r7 r0 r1 r2 r3 r4 r5 r6 r7))))
	  ((16)
	   (putprop name 'isa-stipple rows))
	  (else (ezd-error 'define-stipple
		       "Incorrect number of rows: ~s" (length rows)))))

;;; Define the predefined 4x4 stipples that are named sn and reflect the
;;; fact that n of the 16 bits are set.

(for-each
    define-stipple
    '(         s0
               s1              s2               s3             s4
	       s4a	       s4b		s4c	       s4d
               s5              s6       	s7             s8
	       s8a	       s8b
               s9      	      s10      	       s11  	      s12 
              s13	      s14 	       s15            s16)
    '(      (0 0 0 0)
            (8 0 0 0)       (8 0 2 0)       (#xa 0 2 0)     (#xa 0 #xa 0)
	(#xa 0 #xa 0)   (0 #xa 0 #xa)         (5 0 5 0)         (0 5 0 5)
        (#xa 0 #xa 1)   (#xa 1 #xa 1)     (#xa 5 #xa 1)     (#xa 5 #xa 5)
	(#xa 5 #xa 5)   (5 #xa 5 #xa)
        (#xe 5 #xa 5)   (#xe 5 #xa 7)   (#xe #xd #xa 7)   (#xe #xd #xb 7)
      (#xe #xf #xb 7) (#xe #xf #xf 7) (#xe #xf #xf #xf) (#xf #xf #xf #xf)))

;;; Predicate to test for a stipple and return its bit values when true.

(define (stipple? x) (and (symbol? x) (getprop x 'isa-stipple)))

;;; Users define their own stipples with DEFINE-STIPPLE.

(define-ezd-command
    `(define-stipple ,symbol? (repeat ,integer?))
    "(define-stipple name row-values...)"
    define-stipple)
			    
;;; Font translation from X to Postscript is handled by this table.  Each
;;; X font name is associated with a face and size.

(define *translate-fonts*
    '(("6x10"			"Courier" 10)
      ("6x12"			"Helvetica" 12)
      ("6x13"			"Helvetica" 13)
      ("8x13"			"Courier" 13)
      ("8x13bold" 		"Courier-Bold" 13)
      ("9x15"			"Courier" 15)
      ("fixed"			"Helvetica" 12)
      ("serif10"		"Times-Roman" 10)
      ("serifb10"		"Times-Bold" 10)
      ("serifi10"		"Times-Italic" 10)
      ("sans12"			"Helvetica" 12)
      ("sansb12"		"Helvetica-Bold" 12)
      ("sansi12"		"Helvetica-Oblique" 12)
      ("serif12"		"Times-Roman" 12)
      ("serifb12"		"Times-Bold" 12)
      ("serifi12"		"Times-Italic" 12)

      ("courier8"		"Courier" 8)
      ("courier10"		"Courier" 10)
      ("courier12"		"Courier" 12)
      ("courier14"		"Courier" 14)
      ("courier18"		"Courier" 18)
      ("courier24"		"Courier" 24)

      ("courier_bold8"		"Courier-Bold" 8)
      ("courier_bold10"		"Courier-Bold" 10)
      ("courier_bold12"		"Courier-Bold" 12)
      ("courier_bold14"		"Courier-Bold" 14)
      ("courier_bold18"		"Courier-Bold" 18)
      ("courier_bold24"		"Courier-Bold" 24)

      ("courier_oblique8"	"Courier-Oblique" 8)
      ("courier_oblique10"	"Courier-Oblique" 10)
      ("courier_oblique12"	"Courier-Oblique" 12)
      ("courier_oblique14"	"Courier-Oblique" 14)
      ("courier_oblique18"	"Courier-Oblique" 18)
      ("courier_oblique24"	"Courier-Oblique" 24)

      ("courier_boldoblique8"	"Courier-BoldOblique" 8)
      ("courier_boldoblique10"	"Courier-BoldOblique" 10)
      ("courier_boldoblique12"	"Courier-BoldOblique" 12)
      ("courier_boldoblique14"	"Courier-BoldOblique" 14)
      ("courier_boldoblique18"	"Courier-BoldOblique" 18)
      ("courier_boldoblique24"	"Courier-BoldOblique" 24)

      ("helvetica8"		"Helvetica" 8)
      ("helvetica10"		"Helvetica" 10)
      ("helvetica12"		"Helvetica" 12)
      ("helvetica14"		"Helvetica" 14)
      ("helvetica18"		"Helvetica" 18)
      ("helvetica24"		"Helvetica" 24)

      ("helvetica_bold8"	"Helvetica-Bold" 8)
      ("helvetica_bold10"	"Helvetica-Bold" 10)
      ("helvetica_bold12"	"Helvetica-Bold" 12)
      ("helvetica_bold14"	"Helvetica-Bold" 14)
      ("helvetica_bold18"	"Helvetica-Bold" 18)
      ("helvetica_bold24"	"Helvetica-Bold" 24)

      ("helvetica_oblique8"	"Helvetica-Oblique" 8)
      ("helvetica_oblique10"	"Helvetica-Oblique" 10)
      ("helvetica_oblique12"	"Helvetica-Oblique" 12)
      ("helvetica_oblique14"	"Helvetica-Oblique" 14)
      ("helvetica_oblique18"	"Helvetica-Oblique" 18)
      ("helvetica_oblique24"	"Helvetica-Oblique" 24)

      ("helvetica_boldoblique8"		"Helvetica-BoldOblique" 8)
      ("helvetica_boldoblique10"	"Helvetica-BoldOblique" 10)
      ("helvetica_boldoblique12"	"Helvetica-BoldOblique" 12)
      ("helvetica_boldoblique14"	"Helvetica-BoldOblique" 14)
      ("helvetica_boldoblique18"	"Helvetica-BoldOblique" 18)
      ("helvetica_boldoblique24"	"Helvetica-BoldOblique" 24)

      ("times_roman8"		"Times-Roman" 8)
      ("times_roman10"		"Times-Roman" 10)
      ("times_roman12"		"Times-Roman" 12)
      ("times_roman14"		"Times-Roman" 14)
      ("times_roman18"		"Times-Roman" 18)
      ("times_roman24"		"Times-Roman" 24)

      ("times_bold8"		"Times-Bold" 8)
      ("times_bold10"		"Times-Bold" 10)
      ("times_bold12"		"Times-Bold" 12)
      ("times_bold14"		"Times-Bold" 14)
      ("times_bold18"		"Times-Bold" 18)
      ("times_bold24"		"Times-Bold" 24)

      ("times_italic8"		"Times-Italic" 8)
      ("times_italic10"		"Times-Italic" 10)
      ("times_italic12"		"Times-Italic" 12)
      ("times_italic14"		"Times-Italic" 14)
      ("times_italic18"		"Times-Italic" 18)
      ("times_italic24"		"Times-Italic" 24)

      ("times_bolditalic8"	"Times-BoldItalic" 8)
      ("times_bolditalic10"	"Times-BoldItalic" 10)
      ("times_bolditalic12"	"Times-BoldItalic" 12)
      ("times_bolditalic14"	"Times-BoldItalic" 14)
      ("times_bolditalic18"	"Times-BoldItalic" 18)
      ("times_bolditalic24"	"Times-BoldItalic" 24)))

;;; Define an X to Postscript font translation.

(define (define-font xfont psfont size)
    (set! *translate-fonts* (cons (list xfont psfont size) *translate-fonts*)))

(define-ezd-command
    `(define-font ,string? ,string? ,number?)
    "(define-font \"X11-name\" \"Postscript-name\" size)"
    define-font)

;;; Cursor names are names of characters in the cursor font.  Their names are
;;; in the list *CURSORS*.  The boolean CURSOR-NAME? confirms that a name
;;; is a cursor name.

(define (cursor-name? x)
  (alist-ref x *cursors*))

(define *cursors*)
(letrec-syntax
  ((make-alist (syntax-rules ()
                 ((_) '())
                 ((_ sym . rest) (cons (cons 'sym sym) (make-alist . rest))))))
  (set! *cursors*
    (make-alist XC_NUM_GLYPHS XC_X_CURSOR XC_ARROW XC_BASED_ARROW_DOWN XC_BASED_ARROW_UP
                XC_BOAT XC_BOGOSITY XC_BOTTOM_LEFT_CORNER XC_BOTTOM_RIGHT_CORNER
                XC_BOTTOM_SIDE XC_BOTTOM_TEE XC_BOX_SPIRAL XC_CENTER_PTR XC_CIRCLE
                XC_CLOCK XC_COFFEE_MUG XC_CROSS XC_CROSS_REVERSE XC_CROSSHAIR
                XC_DIAMOND_CROSS XC_DOT XC_DOTBOX XC_DOUBLE_ARROW XC_DRAFT_LARGE
                XC_DRAFT_SMALL XC_DRAPED_BOX XC_EXCHANGE XC_FLEUR XC_GOBBLER XC_GUMBY
                XC_HAND1 XC_HAND2 XC_HEART XC_ICON XC_IRON_CROSS XC_LEFT_PTR
                XC_LEFT_SIDE XC_LEFT_TEE XC_LEFTBUTTON XC_LL_ANGLE XC_LR_ANGLE
                XC_MAN XC_MIDDLEBUTTON XC_MOUSE XC_PENCIL XC_PIRATE XC_PLUS
                XC_QUESTION_ARROW XC_RIGHT_PTR XC_RIGHT_SIDE XC_RIGHT_TEE XC_RIGHTBUTTON
                XC_RTL_LOGO XC_SAILBOAT XC_SB_DOWN_ARROW XC_SB_H_DOUBLE_ARROW
                XC_SB_LEFT_ARROW XC_SB_RIGHT_ARROW XC_SB_UP_ARROW XC_SB_V_DOUBLE_ARROW
                XC_SHUTTLE XC_SIZING XC_SPIDER XC_SPRAYCAN XC_STAR XC_TARGET XC_TCROSS
                XC_TOP_LEFT_ARROW XC_TOP_LEFT_CORNER XC_TOP_RIGHT_CORNER
                XC_TOP_SIDE XC_TOP_TEE XC_TREK XC_UL_ANGLE XC_UMBRELLA XC_UR_ANGLE
                XC_UR_ANGLE XC_WATCH XC_XTERM)))
