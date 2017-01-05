;;; ezd - easy drawing for X11 displays.
;;;
;;; Transparent buttons.

;*              Copyright 1993 Digital Equipment Corporation
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

;;; The TRANSPARENT-CHECK-BUTTON command makes a "transparent" check button in
;;; the current drawing.  The button is a circle drawn in the foreground color
;;; (default is black) that is filled with the clear when the value is false,
;;; or the foreground color when the value is true.  The button text is
;;; written to the right of the button in the foreground color and the
;;; indicated font (default is the default X font).
;;;
;;; When the mouse is not in either the button, or a member of its button set,
;;; the button is "transparent", i.e. drawn with a stipple.  When the mouse is
;;; in the button or a member of its button set, the buttons are drawn solid.
;;; When the button is drawn solid, the background color (default is white) is
;;; used make the button more visible.
;;;
;;; The button's value is complemented by clicking the mouse using button 1
;;; within either the circle or the button text.  The button action is taken
;;; when the button comes up.
;;;
;;; If the button is a member of a radio button set, then clicking on it when
;;; it is set will have no effect.  Clicking an unset button will set it and
;;; clear the set value.
;;;
;;; Button information can be accessed via the following attributes.
;;;
;;;	X
;;;	Y
;;;	WIDTH
;;;	HEIGHT
;;;	ACTION
;;;	TEXT
;;;	FONT
;;;	FOREGROUND
;;;	BACKGROUND
;;;	TRANSPARENT
;;;	VALUE
;;;	RADIO-BUTTON-SET
;;;	BUTTON-SET
;;;
;;;	ATTRIBUTES
;;;	DELETE-OBJECT

(define (make-transparent-check-button button-name x y width height text
	    value action opt-foreground opt-background font)
    
    (define foreground (or opt-foreground 'black))
    (define background (or opt-background 'white))
    (define dy 2)
    (define radio-button-set '())
    (define button-set (list button-name))
    (define drawing-name (current-drawing-name))
    (define transparent #t)
    
    (define ready #f)
    
    (define (enter)
	    (if transparent
		(for-each
		    (lambda (button)
			    (set-attributes drawing-name button
				'(transparent #f)))
		    button-set)))
    
    (define (exit)
	    (when ready
		  (set! value (not value))
		  (set! ready #f)
		  (draw-button))
	    (if (or (not (eq? (cadr *user-event-misc*) drawing-name))
		    (not (memq (caddr *user-event-misc*) button-set)))
		(for-each
		    (lambda (button)
			    (set-attributes drawing-name button
				'(transparent #t)))
		    button-set)))
    
    (define (button-down)
	    (unless (and value (pair? radio-button-set))
		    (set! ready #t)
		    (set! value (not value))
		    (draw-button)))
    
    (define (button-up) 
	    (if ready
		(let ((drawing *user-event-drawing*))
		     (set! ready #f)
		     (ezd '(draw-now))
		     (set! *user-event-misc* (list value))
		     (if (procedure? action) (action) (eval action))
		     (for-each
			 (lambda (button)
				 (if (not (eq? button button-name))
				     (set-attributes drawing button
					 '(value #f))))
			 radio-button-set))
		(draw-button)))

    (define (move-buttons)
	    (move-transparent-button-set *user-event-drawing* button-set
		*user-event-x* *user-event-y*))
    
    (define (draw-button)
	    (ezd '(save-drawing)
		 `(set-drawing ,drawing-name)
		 `(object ,button-name
			  (fill-rectangle ,x ,y ,width ,height
			      ,@(if transparent '(clear) `(,background s8b)))
			  (fill-arc ,x ,y ,(- height dy dy)
			      ,(- height dy dy) 0 360
			      ,(if value foreground 'clear)
			      ,(if transparent 's2 's8b))
			  (arc ,x ,y ,(- height dy dy) ,(- height dy dy)
			       0 360
			       ,(if transparent 'clear foreground)
			       dash)
			  (text ,(+ x height dy dy) ,y ,width ,height left
				center ,text ,foreground
				,(if transparent 's8b 's16)
				,@(if font (list font) '())))
		 '(restore-drawing)))
    
    (define (getattributes)
	    (map (lambda (a)
			 (case a
			       ((x) x)
			       ((y) y)
			       ((width) width)
			       ((height) height)
			       ((text) text)
			       ((action) action)
			       ((font) font)
			       ((foreground) foreground)
			       ((transparent) transparent)
			       ((value) value)
			       ((button-set) button-set)
			       ((radio-button-set) radio-button-set)
			       ((attributes) '(x y width height text action
						 font foreground
						 transparent-stipple value
						 button-set
						 radio-button-set
						 attributes delete-object))
			       (else (ezd-error 'transparent-check-button
					 "Illegal attribute: ~s" a))))
		 *user-event-misc*))
    
    (define (object-list? x)
	    (or (null? x) (and (symbol? (car x)) (object-list? (cdr x)))))
    
    (define (setattributes)
	    (define delete #f)
	    (define redraw #f)
	    (define drawing *user-event-drawing*)
	    
	    (define (set-radio-button-set new-buttons)
		    (for-each
			(lambda (button)
				(if (and (not (memq button new-buttons))
					 (not (eq? button button-name)))
				    (set-attributes drawing button
					'(radio-button-set-value))))
			radio-button-set)
		    (for-each
			(lambda (button)
				(if (not (eq? button button-name))
				    (set-attributes drawing button
					`(radio-button-set-value
					     ,@new-buttons))))
			new-buttons)
		    (set! radio-button-set new-buttons))

	    (for-each
		(lambda (a)
			(cond ((match? (x number?) a)
			       (set! redraw #t)
			       (set! x (cadr a)))
			      ((match? (y number?) a)
			       (set! redraw #t)
			       (set! y (cadr a)))
			      ((match? (width positive-number?) a)
			       (set! redraw #t)
			       (set! width (cadr a)))
			      ((match? (height positive-number?) a)
			       (set! redraw #t)
			       (set! height (cadr a)))
			      ((match? (text string?) a)
			       (set! redraw #t)
			       (set! text (cadr a)))
			      ((match? (action any?) a)
			       (set! action (cadr a)))
			      ((match? (font string?) a)
			       (set! redraw #t)
			       (set! font (cadr a)))
			      ((match? (foreground color?) a)
			       (set! redraw #t)
			       (set! foreground (cadr a)))
			      ((match? (transparent boolean?) a)
			       (set! redraw #t)
			       (set! transparent (cadr a)))
			      ((match? (value boolean?) a)
			       (if (not (equal? value (cadr a)))
				   (set! redraw #t))
			       (set! value (cadr a)))
			      ((and (eq? (car a) 'button-set)
				    (object-list? (cdr a)))
			       (for-each
				   (lambda (obj)
					   (set-attributes drawing-name obj
					       `(button-set-value ,@(cdr a))))
				   (append (cdr a) button-set)))
			      ((eq? (car a) 'button-set-value)
			       (set! button-set (if (memq button-name (cdr a))
						    (cdr a)
						    `(,button-name))))
			      ((and (eq? (car a) 'radio-button-set)
				    (object-list? (cdr a)))
			       (for-each
				   (lambda (obj)
					   (set-attributes drawing-name obj
					       `(radio-button-set-value
						    ,@(cdr a))))
				   (append (cdr a) radio-button-set)))
			      ((eq? (car a) 'radio-button-set-value)
			       (set! radio-button-set
				     (if (memq button-name (cdr a))
					 (cdr a)
					 '())))
			      ((equal? '(delete-object) a)
			       (set! delete #t))
			      (else (ezd-error 'transparent-check-button
					"Illegal attribute: ~s" a))))
		*user-event-misc*)
	    (if delete
		(begin (set-radio-button-set
			   (remq button-name radio-button-set))
		       (ezd `(object ,button-name)
			    `(when ,button-name * #f)))
		(if redraw (draw-button))))
    
    (draw-button)
    (ezd `(when ,button-name get-attributes ,getattributes)
	 `(when ,button-name set-attributes ,setattributes)
	 `(when ,button-name enter ,enter)
	 `(when ,button-name exit ,exit)
	 `(when ,button-name button1down ,button-down)
	 `(when ,button-name button3down ,move-buttons)
	 `(when ,button-name button1up ,button-up)))

(define-ezd-command
    `(transparent-check-button ,symbol? ,number? ,number? ,positive-number?
	 ,positive-number? ,string? ,boolean?
	 ,any? (optional ,color?) (optional ,color?) (optional ,string?))
    "(transparent-check-button name x y width height text value action [foreground [background]] [\"font\"])"
    make-transparent-check-button)

;;; The TRANSPARENT-PUSH-BUTTON command makes a simple button in the current
;;; drawing.  The button is drawn as a foreground colored (default is black)
;;; rectangle.  The button text is written in the center of the button in the
;;; foreground color and font (default is X's).  If several button text's are
;;; provided, then each time the button action is taken, the "next" button
;;; text is displayed.
;;;
;;; When the mouse is not in either the button or a member of it's button set,
;;; the button is "transparent", i.e. drawn with a stipple.  When the mouse is
;;; in it or a member of its button set then it is drawn solid.
;;;
;;; When the mouse enters a button with an action not equal to #f, the
;;; rectangle border solidifies.  When mouse button 1 is pressed, the button
;;; colors are reversed.  When button 1 is released, the button action is taken
;;; and then the button is drawn normally.  When the button action is taken,
;;; the button event type is BUTTON1UP and the miscellaneous information is a
;;; list containing the button text when the button was pressed and the button
;;; text that will next be displayed.  Note that in order for the button
;;; action to be taken, the mouse must remain within the button while the
;;; mouse button is pressed.
;;;
;;; Button information can be accessed via the following attributes.
;;;
;;;	X
;;;	Y
;;;	WIDTH
;;;	HEIGHT
;;;	ACTION
;;;	TEXT
;;;	FONT
;;;	FOREGROUND
;;;	BACKGROUND
;;;	TRANSPARENT
;;;	BUTTON-SET
;;;
;;;	ATTRIBUTES
;;;	DELETE-OBJECT

(define (make-transparent-push-button button-name x y width height text-list
	    action opt-foreground opt-background font)
    
    (define foreground (or opt-foreground 'black))
    (define background (or opt-background 'white))
    (define drawing-name (current-drawing-name))
    (define button-set (list button-name))
    (define transparent #t)
    
    (define bold #f)
    (define invert #f)
    (define textx 0)
    
    (define (enter)
	    (if transparent
		(for-each
		    (lambda (button)
			    (set-attributes drawing-name button
				'(transparent #f)))
		    button-set))
	    (if (and action (not *mouse-button1*)) (draw-button #t #f)))
    
    (define (exit)
	    (draw-button #f #f)
	    (if (or (not (eq? (cadr *user-event-misc*) drawing-name))
		    (not (memq (caddr *user-event-misc*) button-set)))
		(for-each
		    (lambda (button)
			    (set-attributes drawing-name button
				'(transparent #t)))
		    button-set)))
    
    (define (button-down) (if action (draw-button #t #t)))
    
    (define (button-up)
	    (when action
		  (if invert
		      (let ((next-textx (modulo (+ 1 textx)
						(length text-list)))) 
			   (ezd '(draw-now))
			   (set! *user-event-misc*
				 (list (list-ref text-list textx)
				       (list-ref text-list next-textx)))
			   (if (procedure? action) (action) (eval action))
			   (set! textx next-textx)))
		  (draw-button #t #f)))

    (define (move-buttons)
	    (move-transparent-button-set *user-event-drawing* button-set
		*user-event-x* *user-event-y*))
    
    (define (draw-button b i)
	    (set! bold b)
	    (set! invert i)
	    (ezd '(save-drawing)
		 `(set-drawing ,drawing-name)
		 `(object ,button-name
			  (fill-rectangle ,x ,y ,width ,height
			      ,(if transparent
				   'clear
				   (if invert foreground background))
			      s8b)
			  (rectangle ,x ,y ,width ,height 0
			      ,(if transparent 'clear foreground)
			      ,@(if bold '() '(dash)))
			  (text ,x ,y ,width ,height center center
				,(list-ref text-list textx)
				,(if invert background foreground)
				,(if transparent 's8b 's16)
				,@(if font (list font) '())))
		 '(restore-drawing)))
    
    (define (string-list? x)
	    (if (null? x)
		#f
		(let loop ((x x))
		     (if (pair? x)
			 (loop (cdr x))
			 (null? x)))))
    
    (define (getattributes)
	    (map (lambda (a)
			 (case a
			       ((x) x)
			       ((y) y)
			       ((width) width)
			       ((height) height)
			       ((text) text-list)
			       ((action) action)
			       ((font) font)
			       ((foreground) foreground)
			       ((background) background)
			       ((transparent) transparent)
			       ((button-set) button-set)
			       ((attributes) '(x y width height text action
						 font foreground background
						 transparent button-set
						 attributes delete-object))
			       (else (ezd-error 'transparent-push-button
					 "Illegal attribute: ~s" a))))
		 *user-event-misc*))
    
    (define (object-list? x)
	    (or (null? x) (and (symbol? (car x)) (object-list? (cdr x)))))
    
    (define (setattributes)
	    
	    (define (text-list? x)
		    (or (null? x)
			(and (string? (car x)) (text-list? (cdr x)))))
	    
	    (define delete #f)
	    
	    (for-each
		(lambda (a)
			(cond ((match? (x number?) a)
			       (set! x (cadr a)))
			      ((match? (y number?) a)
			       (set! y (cadr a)))
			      ((match? (width positive-number?) a)
			       (set! width (cadr a)))
			      ((match? (height positive-number?) a)
			       (set! height (cadr a)))
			      ((and (eq? (car a) 'text) (text-list? (cdr a))
				    (> (length a) 1))
			       (set! text-list (cdr a)))
			      ((match? (action any?) a)
			       (set! action (cadr a)))
			      ((match? (font string?) a)
			       (set! font (cadr a)))
			      ((match? (foreground color?) a)
			       (set! foreground (cadr a)))
			      ((match? (background color?) a)
			       (set! background (cadr a)))
			      ((match? (transparent boolean?) a)
			       (set! transparent (cadr a)))
			      ((and (eq? (car a) 'button-set)
				    (object-list? (cdr a)))
			       (for-each
				   (lambda (obj)
					   (set-attributes drawing-name obj
					       `(button-set-value ,@(cdr a))))
				   (append (cdr a) button-set)))
			      ((eq? (car a) 'button-set-value)
			       (set! button-set (if (memq button-name (cdr a))
						    (cdr a)
						    `(,button-name))))
			      ((equal? '(delete-object) a)
			       (set! delete #t))
			      (else (ezd-error 'push-button
					"Illegal attribute: ~s" a))))
		*user-event-misc*)
	    (if delete
		(ezd `(object ,button-name) `(when ,button-name * #f))
		(draw-button bold invert)))
    
    (draw-button #f #f)
    (ezd `(when ,button-name get-attributes ,getattributes)
	 `(when ,button-name set-attributes ,setattributes)
	 `(when ,button-name enter ,enter)
	 `(when ,button-name exit ,exit)
	 `(when ,button-name button1down ,button-down)
	 `(when ,button-name button3down ,move-buttons)
	 `(when ,button-name button1up ,button-up)))

(define-ezd-command
    `(transparent-push-button ,symbol? ,number? ,number? ,positive-number?
	 ,positive-number? (repeat ,string?) ,any? (optional, color?)
	 (optional ,color?) (optional ,string?))
    "(transparent-push-button name x y width height text ... action [foreground [background]] [\"font\"])"
    make-transparent-push-button)

;;; Transparent buttons are moved by invoking the following "mover".  It
;;; overlays the window with a transparent drawing and then tracks the mouse,
;;; dragging the button set around as long a mouse button 3 is pressed.

(define (move-transparent-button-set drawing button-set x y)
    
    (define (enter)
	    (if (not *mouse-button3*) (done)))
    
    (define (exit)
	    (if *mouse-button3* (move-objects) (done)))
    
    (define (done)
	    (ezd '(object __transparent-button-mover__)))
    
    (define (move-objects)
	    (let ((dx (- *user-event-x* x))
		  (dy (- *user-event-y* y)))
		 (set! x *user-event-x*)
		 (set! y *user-event-y*)
		 (for-each
		     (lambda (obj)
			     (let ((x (get-attribute drawing obj 'x))
				   (y (get-attribute drawing obj 'y)))
				  (set-attributes drawing obj `(x ,(+ x dx))
				      `(y ,(+ y dy)))))
		     button-set)
		 (ezd '(draw-now))))
    
    
    (ezd `(set-drawing ,drawing)
	 '(object __transparent-button-mover__
		  (fill-rectangle 0 0 1000000 1000000 clear))
	 '(float __transparent-button-mover__)
	 `(when __transparent-button-mover__ enter ,enter)
	 `(when __transparent-button-mover__ exit ,exit)
	 `(when __transparent-button-mover__ button3up ,done)
	 `(when __transparent-button-mover__ motion ,move-objects)))
    
