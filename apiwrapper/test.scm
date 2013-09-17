#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -- $0 "$@"
(use gauche.test)
(use routing-client)
(use gauche.net)

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

(test-start "osm web service")
(test* "simple test cycling"
       44
       (request '(8.92 48.58) '(9.28 48.40) 'cycling))
(test* "simple test hiking"
       49
       (request '(8.92 48.58) '(9.28 48.40) 'hikingTourTrail))
(test* "simple test mountainbike"
       46
       (request '(8.92 48.58) '(9.28 48.40) 'mountainbiking))
(test-end)
