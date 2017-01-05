;;; ezd - easy drawing for X11 displays.
;;;
;;; A SLIDER is a horizontal or vertical rectangular area with a movable
;;; indicator.  The operations supported are:
;;;
;;;	Click button 1 on background	move indicator one "jump" toward
;;;					the mouse and then evaluate the	action.
;;;
;;;	Click button 2 on background	move indicator to that position and
;;;					then evaluate the action.
;;;
;;;	Drag indicator with button 1	follow the mouse, evaluating the action
;;;					as it moves.
;;;
;;; The following attributes are accessible:
;;;
;;;	X
;;;	Y
;;;	WIDTH
;;;	HEIGHT
;;;	INDICATOR-SIZE
;;;	MIN-VALUE
;;;	MAX-VALUE
;;;	VALUE
;;;	JUMP-SIZE
;;;	ACTION
;;;	FOREGROUND
;;;	BACKGROUND
;;;	FOREGROUND-STIPPLE
;;;
;;;	DELETE-OBJECT
;;;	ATTRIBUTES

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

(define (make-slider name x y width height indicator-size min-value max-value
	    value jump-size action foreground-color background-color stipple)
    
    (define foreground (or foreground-color 'black))
    (define background (or background-color 'white))
    
    (define vertical (< width height))
    
    (define fixed-value (and (fixnum? indicator-size) (fixnum? min-value)
			     (fixnum? max-value) (fixnum? value)
			     (fixnum? jump-size)))
    
    (define pad (if (< width height) (quotient width 15) (quotient height 15)))
    
    (define inside #f)
    
    (define was-inside #f)

    (define restore-cursor #f)
    
    (define user-range (+ indicator-size (- max-value min-value)))
    
    (define indicator
	    (ceiling (* (max height width) (/ indicator-size user-range))))
    
    (define indicator-name (string->symbol (string-append (symbol->string name)
					       "-indicator")))
    
    (define (value->d)
	    (* (max height width) (/ (- value min-value) user-range)))
    
    (define (mouse->value)
	    (let ((value  (+ (* user-range
				(/ (if vertical
				       (- *user-event-y* y)
				       (- *user-event-x* x))
				   (max height width)))
			     (- min-value (/ indicator-size 2)))))
		 (if fixed-value (inexact->exact (round value)) value)))
    
    (define (draw-background)
	    (ezd `(object ,name
			  (fill-rectangle ,x ,y ,width ,height ,foreground)
			  ,(if vertical
			       `(fill-rectangle ,(+ x pad) ,y
				    ,(- width pad pad) ,height ,background)
			       `(fill-rectangle ,x ,(+ y pad)
				    ,width ,(- height pad pad) ,background)))))
    
    (define (draw-indicator)
	    (ezd `(object ,indicator-name
			  ,(if vertical
			       `(fill-rectangle ,x ,(+ y (value->d))
				    ,width ,indicator ,foreground
				    ,@(if stipple (list stipple) '()))
			       `(fill-rectangle ,(+ x (value->d)) ,y
				    ,indicator ,height ,foreground
				    ,@(if stipple (list stipple) '()))))))
    
    (define (enter)
	    (cond (inside)
		  ((and (not *mouse-button1*) (not *mouse-button2*))
		   (set! inside #t)
		   (set! restore-cursor `(restore-cursor ,*user-event-window*))
		   (ezd `(save-cursor ,*user-event-window*)
			`(set-cursor ,*user-event-window*
			     ,(if vertical
				  'xc_sb_v_double_arrow
				  'xc_sb_h_double_arrow))))
		  ((and *mouse-button1* (not *mouse-button2*)
			(eq? was-inside 'on))
		   (set! inside 'on)
		   (set! restore-cursor `(restore-cursor ,*user-event-window*))
		   (ezd `(save-cursor ,*user-event-window*)
			`(set-cursor ,*user-event-window*
			     ,(if vertical
				  'xc_sb_v_double_arrow
				  'xc_sb_h_double_arrow)))
		   (motion))))
    
    (define (exit)
	    (when (and inside
		       (not (and (eq? *user-event-window*
				      (car *user-event-misc*))
				 (eq? *user-event-drawing*
				      (cadr *user-event-misc*))
				 (or (eq? name (caddr *user-event-misc*))
				     (eq? indicator-name
					  (caddr *user-event-misc*))))))
		  (set! was-inside inside)
		  (set! inside #f)
		  (set! restore-cursor #f)
		  (ezd `(restore-cursor ,*user-event-window*))))
    
    (define (button1down)
	    (let ((mv (mouse->value))
		  (ind2 (/ indicator-size 2)))
		 (cond ((<= mv (- value ind2)) (set! inside 'before))
		       ((>= mv (+ value ind2)) (set! inside 'after))
		       (else (set! inside 'on)))))
    
    (define (take-action new-value)
	    (set! value (max min-value (min max-value new-value)))
	    (draw-indicator)
	    (set! *user-event-misc* (list value))
	    (if (procedure? action) (action) (eval action))
 	    (ezd '(draw-now)))
   
    (define (button1up)
	    (case inside
		  ((before) (take-action (- value jump-size)))
		  ((after) (take-action (+ value jump-size)))
		  ((on) #t)
		  (else (enter))))
    
    (define (button2up) (if inside (take-action (mouse->value)) (enter)))
    
    (define (motion)
	    (if (and *mouse-button1* (eq? inside 'on))
		(take-action (mouse->value))))
    
    (define (get-attributes)
	    (map (lambda (a)
			 (case a
			       ((x) x)
			       ((y) y)
			       ((width) width)
			       ((height) height)
			       ((indicator-size) indicator-size)
			       ((min-value) min-value)
			       ((max-value) max-value)
			       ((value) value)
			       ((jump-size) value)
			       ((action) action)
			       ((foreground) foreground)
			       ((background) background)
			       ((foreground-stipple) stipple)
			       ((attributes) '(x y width height indicator-size
						 min-value max-value value
						 jump-size action foreground
						 background foreground-stipple
						 delete-object attributes))
			       (else (ezd-error 'check-button
					 "Illegal attribute: ~s" a))))
		 *user-event-misc*))
    
    (define (set-attributes)
	    (let ((delete #f))
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
				   ((match? (indicator-size non-negative?) a)
				    (set! indicator-size (cadr a)))
				   ((match? (min-value number?) a)
				    (set! min-value (cadr a)))
				   ((match? (max-value number?) a)
				    (set! max-value (cadr a)))
				   ((match? (value number?) a)
				    (set! value (cadr a)))
				   ((match? (jump-size non-negative?) a)
				    (set! jump-size (cadr a)))
				   ((match? (action any?) a)
				    (set! action (cadr a)))
				   ((match? (foreground color?) a)
				    (set! foreground (cadr a)))
				   ((match? (background color?) a)
				    (set! background (cadr a)))
				   ((match? (foreground-stipple stipple?) a)
				    (set! stipple (cadr a)))
				   ((equal? '(delete-object) a)
				    (set! delete #t))
				   (else (ezd-error 'slider
					     "Illegal attribute: ~s" a))))
		     *user-event-misc*)
		 (if restore-cursor (ezd restore-cursor))
		 (if delete
		     (ezd `(object ,name)
			  `(when ,name * #f)
			  `(object ,indicator-name)
			  `(when ,indicator-name * #f))
		     (make-slider name x y width height indicator-size
			 min-value max-value value jump-size action
			 foreground background stipple))))
    
    (draw-background)
    (draw-indicator)
    (ezd `(when ,indicator-name enter ,enter)
	 `(when ,indicator-name exit ,exit)
	 `(when ,indicator-name button1down ,button1down)
	 `(when ,indicator-name button1up ,button1up)
	 `(when ,indicator-name button2up ,button2up)
	 `(when ,indicator-name motion ,motion)
	 `(when ,name enter ,enter)
	 `(when ,name exit ,exit)
	 `(when ,name button1down ,button1down)
	 `(when ,name button1up ,button1up)
	 `(when ,name button2up ,button2up)
	 `(when ,name motion ,motion)
	 `(when ,name get-attributes ,get-attributes)
	 `(when ,name set-attributes ,set-attributes)))

(define-ezd-command
    `(slider ,symbol? ,number? ,number? ,positive-number? ,positive-number?
	     ,positive-number? ,number? ,number? ,number? ,non-negative?
	     ,any? (optional ,color?) (optional ,color?) (optional ,stipple?))
    "(slider name x y width height indicator-size min-value max-value value jump-size action [foreground [background]] [stipple])"
    make-slider)
