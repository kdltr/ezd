;;; ezd - easy drawing for X11 displays.
;;;
;;; Command parsing and definition.

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

;(declare (unit commands))
;(include "scheme2c")
;(include "commands.sch")

;;; COMMAND PARSING.

;;; Helper functions for command argument parsing.

(define (ACONS x y) (if (eq? #f y) #f (cons x y)))

(define (MATCHED-ARGS x y)
    (define (match-list x y)
	    (if (null? x)
		'()
		(cons (if (pair? y) (car y) '())
		      (match-list (cdr x) (if (pair? y) (cdr y) '())))))
    (if (= (length x) 1) (car y) (match-list x y)))

(define (REST-ARGS x y)
    (if (or (null? x) (null? y)) y (rest-args (cdr x) (cdr y))))

;;; Command parsing is handled by the following procedure.  Its arguments are
;;; a template describing the command arguments and the actual command
;;; arguments.  It returns either a list of parsed arguments or #F.  The
;;; template is a list composed of any of the following elements that are
;;; matched as noted:
;;;
;;;				MATCHES WHEN TRUE:
;;;
;;;	(OPTIONAL <template>)	#t
;;;
;;;	(REPEAT <template>)	#t
;;;
;;;	(OR <template>...)	#t
;;;
;;;	*REST*			#t
;;;
;;;	<a procedure>		(<a procedure> argument)
;;;
;;;	<any other obj>		(EQUAL? <any other obj> argument)
;;;
;;; When the command can be parsed, the result is a list of arguments
;;; generated by the matches to the template.  Template items contribute to
;;; the result as follows:
;;;				RETURNS ON A MATCH:
;;;
;;;	(OPTIONAL <template>)	either #f when the <template> could not be
;;;				matched, or the item it matched (<template>
;;;				consists of one item), or a list of items
;;;				matched.
;;;
;;;	(REPEAT <template>)	a list of items matched (<template> consists
;;;				of one item), or a list of lists of items
;;;				matched.
;;;
;;;	(OR <template>...) 	the result of the first <template> that matches
;;;				or #f when no <template> matched.
;;;
;;;	*REST*			a list of the remaining arguments.
;;;
;;;	<a procedure>		the argument.
;;;
;;;	<any other obj>		no value is returned.

(define (ARG-PARSE template args)
    (if template
	(let ((x (car template)))
	     (cond ((and (pair? x) (eq? (car x) 'optional))
		    (let ((match (arg-parse (append (cdr x) (cdr template))
				     args)))
			 (if (not (eq? match #f))
			     (acons (matched-args (cdr x) args)
				    (arg-parse (cdr template)
					(rest-args (cdr x) args)))
			     (acons #f (arg-parse (cdr template) args)))))
		   ((and (pair? x) (eq? (car x) 'repeat))
		    (let loop ((found '()) (args args))
			 (let ((match (arg-parse
					  (append (cdr x) '(*rest*)) args)))
			      (if (not (eq? match #f))
				  (loop (append found
						(list (matched-args (cdr x)
							  match)))
					(rest-args (cdr x) match))
				  (acons found
					 (arg-parse (cdr template) args))))))
		   ((and (pair? x) (eq? (car x) 'or))
		    (let loop ((tl (cdr x)))
			 (if (null? tl)
			     #f
			      (let ((match (arg-parse
					        (append (car tl) '(*rest*))
						args)))
				   (if (not (eq? match #f))
				       (acons (matched-args (car tl) match)
					      (arg-parse (cdr template)
						  (rest-args (car tl) match)))
				       (loop (cdr tl)))))))
		   ((eq? x '*rest*) args)
		   ((null? args) #f)
		   ((procedure? x)
		    (let ((arg (car args)))
			 (if (x arg)
			     (acons arg (arg-parse (cdr template) (cdr args)))
			     #f)))
		   ((equal? x (car args))
		    (arg-parse (cdr template) (cdr args)))
		   (else #f)))
	(if (null? args) '() #f)))

;;; Generally useful predicates for command decoding.

(define (NON-NEGATIVE? x) (and (number? x) (>= x 0)))

(define (NON-ZERO? x) (and (number? x) (not (= x 0))))

(define (POSITIVE-NUMBER? x) (and (number? x) (> x 0)))

#;(define (ANY? x) #t) ;; Already defined in CHICKEN

(define (DASH? x) (eq? x 'dash))

;;; ezd commands are defined by calls to the following procedure.  The caller
;;; provides the command name (a symbol), the argument parsing template, a
;;; string describing the correct form of the command, and the action procedure
;;; that is to be called when the command is successfully parsed.

(define EZD-COMMANDS '())

(define (DEFINE-EZD-COMMAND template description action)
    (let* ((command (car template))
	   (x (assoc command ezd-commands)))
	  (if x (set! ezd-commands (delete x ezd-commands)))
	  (set! ezd-commands
		(cons (list command template description action) ezd-commands))
	  command))

;;; Errors in ezd commands are reported by calling the procedure EZD-ERROR.
;;; This will result in either the message being logged to the stderr-port, or
;;; the Scheme error handler error being called.

(define IN-READ-EVAL-DRAW #f)

(define (EZD-ERROR id form . args)
    (if (not in-read-eval-draw) (apply error id form args))
    (apply format (current-error-port) form args)
    (newline (current-error-port))
    #f)

;;; Module initialization procedure.

(define (COMMANDS-MODULE-INIT)
    (set! in-read-eval-draw #f)
    #t)
