;;; A Clock face.  Drag either of the hands with mouse button 1 to set the
;;; value.  The clock face is drawn with its center at 0,0 in a cartesian
;;; (Y goes up) coordinate system.  Click button 3 on the background to quit.
;;; To run:
;;;
;;; 	csi -s clock.scm

(import ezd)

(define (clock time)
    
    (define pi 3.14159)
    (define pi*2 (* 3.14159 2))
    (define pi/2 (/ 3.14159 2))
    
    (define (zero-2pi x) (if (< x 0) (+ x pi*2) x))
    
    (define (minute->angle minute) (zero-2pi (- pi/2 (* (/ minute 30) pi))))
    
    (define (angle->minute angle) (modulo (- 15 (/ (* 30 angle) pi)) 60))
    
    (define (xy->angle x y) (zero-2pi (atan y x)))
    
    (define (draw-hand name length minute)
	    (let ((angle (minute->angle minute)))
		 (ezd `(object ,name
			       (fill-polygon 0 0
				   ,(* 25 (cos (+ angle .25)))
				   ,(* 25 (sin (+ angle .25)))
				   ,(* length (cos angle))
				   ,(* length (sin angle))
			           ,(* 25 (cos (- angle .25)))
				   ,(* 25 (sin (- angle .25))))))))
    
    (define (draw-hands)
	    (draw-hand 'minute 95 (remainder (round time) 60))
	    (draw-hand 'hour 65 (remainder (round (/ time 12)) 60)))
    
    (define mark-hand #f)
    (define mark-angle #f)
    
    (define (mark)
	    (set! mark-hand *user-event-object*)
	    (set! mark-angle (minute->angle
				 (remainder (round (if (eq? mark-hand 'hour)
						(/ time 12)
						time))
				     60)))
	    (ezd '(object cover (fill-rectangle -100 -100 200 200 clear))))
    
    (define (cover-enter) (if (not *mouse-button1*) (ezd '(object cover))))
    
    (define (cover-button1up) (ezd '(object cover)))
    
    (define (cover-motion)
	    (let* ((new-angle (xy->angle *user-event-x* *user-event-y*))
		   (delta-angle (cond ((< (- mark-angle new-angle) (- pi))
				       (+ (- mark-angle new-angle) pi*2))
				      ((> (- mark-angle new-angle) pi)
				       (- (- mark-angle new-angle) pi*2))
				      (else (- mark-angle new-angle))))
		   (delta-t (inexact->exact
				(round (* delta-angle (/ 30 pi)
				   (if (eq? mark-hand 'hour) 12 1))))))
		  (unless (zero? delta-t)
			  (set! time (modulo (+ time delta-t) 720))
			  (if (eq? mark-hand 'hour)
			      (set! mark-angle (minute->angle (/ time 12)))
			      (set! mark-angle (minute->angle
						   (remainder (round time) 60))))
			  (draw-hands))))
    
    (ezd '(window clock-window 200 200 fixed-size)
	 '(set-drawing clock)
	 '(overlay clock-window clock)
	 '(origin clock-window clock 100 100)
	 '(scale clock-window clock 1 -1 1)
	 '(object back (fill-arc -100 -100 200 200 0 360 gray95)
		  (arc -100 -100 200 200 0 360 gray85))	 
	 '(object minute)
	 '(text -60 -60 120 120 left up "time" grey60 "times_italic24")
	 '(text -60 -60 120 120 right center "drifts" grey60
		"times_italic24")
	 '(text -60 -60 120 120 left down "by" grey60 "times_italic24")
	 '(object hour)
	 '(fill-arc -5 -5 10 10 0 360 black)
         '(object cover)
	 '(when back button2down (ezd '(postscript clock-window "clock.psf")))
	 '(when back button3down (ezd '(quit)))
	 `(when minute button1down ,mark)
	 `(when hour button1down ,mark)
	 `(when cover enter ,cover-enter)
	 `(when cover button1up ,cover-button1up)
	 `(when cover motion ,cover-motion))
    (draw-hands))

(clock 45)
(ezd '(pause))