;;; The clock example, using an external ezd process instead of the module.
;;; 	csi -s clock-external.scm

(use ezd-external matchable)

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

(define time 23)

(define (draw-hands)
  (draw-hand 'minute 95 (remainder (round time) 60))
  (draw-hand 'hour 65 (remainder (round (/ time 12)) 60)))

(define mark-hand #f)
(define mark-angle #f)

(define (mark hand)
  (set! mark-hand hand)
  (set! mark-angle (minute->angle
                                  (remainder (round (if (eq? mark-hand 'hour)
                                                        (/ time 12)
                                                        time))
                                             60)))
  (ezd '(object cover (fill-rectangle -100 -100 200 200 clear))))


(define (cover-motion x y )
  (let* ((new-angle (xy->angle x y))
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
          (set! mark-angle (minute->angle (remainder (round time) 60))))
      (draw-hands))))

(ezd '(window clock-window 200 200 fixed-size)
     '(set-drawing clock)
     '(overlay clock-window clock)
     '(origin clock-window clock 100 100)
     '(scale clock-window clock 1 -1 1)
     '(object back
              (fill-arc -100 -100 200 200 0 360 gray95)
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
      '(when back button3down (log-event))
      `(when minute button1down (log-event))
      `(when hour button1down (log-event))
      `(when cover enter (if (not *mouse-button1*) (ezd '(object cover))))
      `(when cover button1up (ezd '(object cover)))
      `(when cover motion (log-event)))

(draw-hands)
(let loop ((ev (read ezd-input-port)))
  (unless (eof-object? ev)
    (match ev
      (('button3down 'clock-window 'clock 'back . _)
       (ezd '(quit))
       (exit 1))
      (('button1down 'clock-window 'clock hand _ _ _ _)
       (mark hand))
      (('motion 'clock-window 'clock 'cover x y _ _)
       (cover-motion x y)))
    (loop (read ezd-input-port))))
