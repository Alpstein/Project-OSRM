#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -- $0 "$@"

(use sxml.adaptor) ;; for assert
(use svg-plot)
(use srfi-1) ;; for filter
(use util.list) ;; for assoc-ref
(use gauche.sequence)

(define (main args)
  (debug-print-width 10000)
  (let ((script-run #f)
        (stats #f)
        (cmd #f)
        (res (list)))
    (until (read-line) eof-object? => line
           (if (not cmd)
             (cond [(#/Script started on (.*)/ line)
                    => (lambda (m)
                         (assert (not stats))
                         (set! stats (list (list 'in-ways 0) (list 'out-ways 0)))
                         (set! script-run `((date ,(m 1)))))]
                   [(#/Script done on/ line)
                    (set! stats #f)]
                   [(#/splited \d+\/(\d+) ways into (\d+) ways/ line)
                    => (lambda (m)
                         (assert stats)
                         (inc! (cadar stats) (string->number (m 1)))
                         (inc! (cadadr stats) (string->number (m 2))))]
                   [(#/Command being timed: \"([^ ]+) ([^\"]+)\"/ line)
                    => (lambda (m)
                         (set! cmd `((cmd ,(m 1) ,(m 2)))))])
             (cond [(#/Elapsed \(wall clock\) time \(h:mm:ss or m:ss\): ((\d*):)?(\d+):(\d+(.\d+)?)/ line)
                    => (lambda (m)
                         (push! cmd `(wall-clock ,(+ (* (+ (* (or (and (m 2) (string->number (m 2))) 0) 60)
                                                           (string->number (m 3)))
                                                        60)
                                                     (string->number (m 4))))))]
                   [(#/Maximum resident set size \(kbytes\): (\d+)/ line)
                    => (lambda (m)
                         (push! cmd `(max-rss ,(* (string->number (m 1)) 1024))))]
                   [(#/Major \(requiring I\/O\) page faults: (\d+)/ line)
                    => (lambda (m)
                         (push! cmd `(maj-faults ,(string->number (m 1)))))]
                   [(#/Exit status: / line)
                    (push! res (append script-run (reverse cmd) stats))
                    (set! cmd #f)])))
    ;; (for-each (lambda(r)
    ;;             (write r)
    ;;             (newline))
    ;;           (reverse res))
    (let1 test-runs (group-sequence res :key (lambda(x) (car (assoc-ref x 'out-ways))))
      ;; (assert (= (size-of test-runs) 10))
      (assert (every (cute = 3 <>) (map size-of test-runs)))
      (svg-plot (list (map (lambda(test-run)
                             (list (car (assoc-ref (car test-run) 'out-ways))
                                   (apply +
                                          (map (lambda(x)
                                                 (car (assoc-ref x 'wall-clock)))
                                               test-run))))
                           test-runs))
                :titles (list "wall-clock")))
    ;; (let1 plots-y
    ;;     ;; (list (list identity (cute /. <> 1e8)) '(wall-clock max-rss))
    ;;     ;;(list (list identity) '(max-rss))
    ;;     (list (list identity) '(wall-clock))
    ;;   (svg-plot (map (lambda(f s)
    ;;                    (map (lambda(x)
    ;;                           (list (car (assoc-ref x 'in-ways))
    ;;                                 (f (car (assoc-ref x s)))))
    ;;                         (filter (lambda(x)
    ;;                                   (and (string=? (car (assoc-ref x 'cmd)) "./build/osrm-extract")
    ;;                                        (#/\/foot/ (cadr (assoc-ref x 'cmd))))
    ;;                                   ;;(string=? (car (assoc-ref x 'cmd)) "./waysplit.sh")
    ;;                                   )
    ;;                                 res)))
    ;;                  (car plots-y)
    ;;                  (cadr plots-y))
    ;;             :titles (map symbol->string (cadr plots-y))))
    )
  0)
