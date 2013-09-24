#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -- $0 "$@"
(use routing-client)
(use gauche.net)
(use srfi-27)
(use util.list) ;; assoc-ref
(use gauche.time)
(use srfi-1)
(use slib)
(require 'charplot)

(random-source-randomize! default-random-source)

;; do dns lookup only once, see also discussion about sys-gethostbyname on gauche-devel ml
(define *host* (car (slot-ref (sys-gethostbyname "routing.outdooractive.com.") 'addresses)))
(define *path* "/r/osrm")
(define *service* `(,|*host*| ,|*path*|))

(debug-print-width 4000)
(define (request from to preset)
  (let1 r (routing-request-js 4326
                              from
                              to
                              `((costff . ,#`"(sport-preset ,preset network)"))
                              (list *host* *path*))
    (and (equal? (assoc-ref r "status") "OK")
         (round->exact (/ (assoc-ref (assoc-ref (ref (assoc-ref (ref (assoc-ref r "routes") 0) "legs") 0) "distance") "value")
                          1000)))))

(define (random-point)
  (map + '(8 46) (map (cute * <> 8) (list (random-real) (random-real)))))

;; todo: use new gauche function time-these?
(define (timed thunk)
  (let1 t (make <real-time-counter>)
	(let1 r (with-time-counter t
				   (thunk))
	      (values r
		      (time-counter-value t)))))

(define (avg . l)
  (/. (apply + l)
      (length l)))

(define (print-stats s l)
  (histograph l s)
  (for-each (lambda(f)
	      (format #t "~a=~a "
		      (ref f 'info)
		      (apply f l)))
	    (list min avg max +))
  (newline))

(define (main args)
  (let1 results (map (lambda(i)
		       (let ((from (random-point))
			     (to   (random-point)))
			 (map (lambda(s)
				(receive (r t) (timed (cute request from to s))
					 (list s r t)))
			      '(cycling hikingTourTrail
					;;mountainbiking
					))))
		     (iota 50))
	(print-stats "km" (filter number?
				  (append (map cadar results)
					  (map cadadr results))))
	(plot (filter (compose number? car)
		      (map list
			   (append (map cadar results)
				   (map cadadr results))
			   (append (map caddar results)
				   (map (compose caddr cadr) results))))
	      "km"
	      "s")
	(print-stats "time" (append (map caddar results)
				    (map (compose caddr cadr) results)))
	)
  0)

