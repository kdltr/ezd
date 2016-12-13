;;; Shows a window and terminates after a button-press 
;;  (or after the second exposure event)
 
(use xlib srfi-18)

(define (ynextevent display event)
    (thread-wait-for-i/o! (xconnectionnumber display) #:input)
    (xnextevent display event))

 
(define disp (xopendisplay #f))
(assert disp)

(define screen (xdefaultscreen disp))

(define root (xrootwindow disp screen))
(define window (xcreatesimplewindow
 		 disp root 100 200 300 50 0
 		 (xblackpixel disp screen)
 		 (xwhitepixel disp screen)))

(assert window)

(define gc (xcreategc disp window 0 #f))
(define fd (xconnectionnumber disp))
(define event (make-xevent))

(xsetforeground disp gc (xblackpixel disp screen))
(xsetbackground disp gc (xwhitepixel disp screen))
(xsetfunction disp gc GXCOPY)
(xselectinput disp window (bitwise-ior EXPOSUREMASK BUTTONPRESSMASK))
(xmapwindow disp window)
(xnextevent disp event)

(define winthread
    (thread-start!
        (lambda ()
            (let loop ()
                (xdrawstring disp window gc 100 30 "Hello World!" 12)
                (xflush disp)
                (ynextevent disp event)
                (print "looping!")
                (loop)))))
