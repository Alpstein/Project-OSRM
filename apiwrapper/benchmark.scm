#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -- $0 "$@"
(use routing-client)
(use gauche.net)
(use srfi-27)
(use util.list) ;; assoc-ref

;; (random-source-randomize! default-random-source)

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
  (map + '(9 48.5) (list (random-real) (random-real))))

(define (main args)
  (dotimes (i 100)
    (let ((from (random-point))
          (to   (random-point)))
      (for-each (lambda(s)
                  (time #?=(request from to s)))
                '(cycling hikingTourTrail mountainbiking))))
  0)
