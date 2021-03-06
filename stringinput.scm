;;; ezd - easy drawing for X11 displays.
;;;
;;; The procedures in this module implement simple keyboard entry.

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

;;; A keyboard input object is defined by the following command.  When the
;;; cursor is within the input object (a user defined rectangle), it will
;;; become a pencil.  Characters typed at this time will be collected and
;;; displayed in the input area.  When the return key is pressed, the user
;;; action is executed.  On execution of the user action, *USER-EVENT-TYPE* is
;;; STRING-INPUT and *USER-EVENT-MISC* is the input string.
;;;
;;; Once the object has been created, the following attributes may be accessed
;;; and set.
;;;
;;;	X
;;;	Y
;;;	WIDTH
;;;	HEIGHT
;;;	ACTION
;;;	TEXT
;;;	TEXT-COLOR
;;;	FONT
;;;
;;;	DELETE-OBJECT
;;;	ATTRIBUTES

(define (string-input object-name x y width height text action text-color font)
    
    (define control-u (list->string (list (integer->char 21))))
    (define control-h (list->string (list (integer->char 8))))
    (define bskey (list->string (list (integer->char 127))))
    (define eol (list->string (list (integer->char 13))))
    
    (define (draw-text)
	    (ezd `(object ,object-name
			  (fill-rectangle ,x ,y ,width ,height clear)
			  (text ,x ,y ,width ,height left center
				,(let loop ((text text))
				      (if (or (eq? text "")
					      (>= xwidth
						  (cadr (text->height-width
							    text font))))
					  text
					  (loop (substring text 1
						    (string-length text)))))
				,@(if text-color (list text-color) '())
				,@(if font (list font) '())))))
    
    (define (keyin)
	    (let ((char (car *user-event-misc*)))
		 (cond ((equal? char control-u)
			(set! text "")
			(draw-text))
		       ((and (or (equal? char control-h) (equal? char bskey)))
			(when (not (equal? text ""))
			      (set! text (substring text 0
					     (- (string-length text) 1)))
			      (draw-text)))
		       ((equal? char eol)
			(set! *user-event-type* 'string-input)
			(set! *user-event-misc* (list text))
			(ezd '(draw-now))
			(if (procedure? action) (action) (eval action)))
		       (else (set! text (string-append text char))
			     (draw-text)))))
    
    (define xwidth width)
    
    (define (visible)
	    (if (car *user-event-misc*)
		(set! xwidth
		      ((view-user->width
			   (window-drawing->view *user-event-window*
			       *user-event-drawing*)) width))))
    
    (define (enter-object)
	    (ezd `(save-cursor ,*user-event-window*)
		 `(set-cursor ,*user-event-window* XC_PENCIL)))
    
    (define (exit-object)
	    (ezd `(restore-cursor ,*user-event-window*)))
    
    (define (get-attributes)
	    (map (lambda (a)
			 (case a
			       ((x) x)
			       ((y) y)
			       ((width) width)
			       ((height) height)
			       ((action) action)
			       ((text) text)
			       ((text-color) text-color)
			       ((font) font)
			       ((attributes) '(x y width height action text
						 text-color font attributes
						 delete-object))
			       (else (ezd-error 'string-input
					 "Invalid attribute: ~s" a))))
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
				   ((match? (action any?) a)
				    (set! action (cadr a)))
				   ((match? (text string?) a)
				    (set! text (cadr a)))
				   ((match? (text-color color?) a)
				    (set! text-color (cadr a)))
				   ((match? (font string?) a)
				    (set! font (cadr a)))
				   ((equal? '(delete-object) a)
				    (set! delete #t))
				   (else (ezd-error 'string-input
					     "Invalid attribute: ~s" a))))
		     *user-event-misc*)
		 (if (not delete)
		     (draw-text)
		     (ezd `(object ,object-name) `(when ,object-name * #f)))))
    
    (draw-text)
    (ezd `(when ,object-name keypress ,keyin)
	 `(when ,object-name enter ,enter-object)
	 `(when ,object-name exit ,exit-object)
	 `(when ,object-name visible ,visible)
	 `(when ,object-name get-attributes ,get-attributes)
	 `(when ,object-name set-attributes ,set-attributes)))

(define-ezd-command
    `(string-input ,symbol? ,number? ,number? ,number? ,number? ,string? ,any?
	 (optional ,color?) (optional ,string?))
    "(string-input object-name x y width height \"initial\" action [<color>] [<\"font\">])"
    string-input)
