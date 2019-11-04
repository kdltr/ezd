;;;	A simple 4x4 puzzle game.  Click on a tile to move it into the
;;;	adjacent empty space.  Type control-c to exit.
;;;
;;; To run this program:
;;;
;;;	csc -s puzzle.scm

(import (chicken format) ezd)
 
(define (puzzle pause)
    (define (control-c-exit)
	    (if (equal? (list->string (list (integer->char 3)))
			(car *user-event-misc*))
		(ezd '(quit))))
    (ezd `(window puzzle ,puzzle-size ,puzzle-size fixed-size)
	 '(set-drawing puzzle)
	 '(overlay puzzle puzzle)
	 `(object backing (fill-rectangle 0 0 ,puzzle-size ,puzzle-size white))
	 `(when * keypress ,control-c-exit))
    (draw-puzzle)
    (if pause (ezd '(pause))))

(define tile-size 40)
(define (tile->pixel x) (+ (* (+ x 1) 5) (* x tile-size)))
(define puzzle-size (tile->pixel 4))

(define (draw-puzzle)
    (define zero-x 0)
    (define zero-y 0)

    (define (draw-tile x y tile)
	    (let ((tile-name (string->symbol (format "TILE~s" tile))))
		 (define (draw-tile)
			 (ezd `(object ,tile-name
				       (fill-rectangle
					   ,(tile->pixel x) ,(tile->pixel y)
					   ,tile-size ,tile-size blue)
				       (text ,(tile->pixel x) ,(tile->pixel y)
					     ,tile-size ,tile-size center
					     center ,(format "~s" tile)
					     white "8x13bold"))))
		 (define (clicker)
			 (when  (= (+ (abs (- x zero-x)) (abs (- y zero-y))) 1)
				(let ((zx zero-x)
				      (zy zero-y))
				     (set! zero-x x)
				     (set! zero-y y)
				     (set! x zx)
				     (set! y zy))
				(draw-tile)))
		 (draw-tile)
		 (ezd `(click ,tile-name 1 ,clicker))))

    (let ((tiles (list 10 15 12 3 13 8 7 1 2 14 6 4 9 5 11)))
	 (do ((x 0 (+ x 1)))
	     ((= x 4))
	     (do ((y (if (= x 0) 1 0) (+ y 1)))
		 ((= y 4))
		 (draw-tile x y (car tiles))
		 (set! tiles (cdr tiles))))))

(puzzle #t)