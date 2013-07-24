#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -- $0 "$@"

(use sxml.adaptor) ;; for assert

(define (main args)
  (let ((script-run #f)
        (stats #f)
        (cmd #f)
        (res (list)))
    (until (read-line) eof-object? => line
           (if (not cmd)
             (cond [(#/Script started on (.*)/ line)
                    => (lambda (m)
                         (set! stats (list 0 0))
                         (set! script-run (list (m 1))))]
                   [(#/splited \d+\/(\d+) ways into (\d+) ways/ line)
                    => (lambda (m)
                         ;;#?=(m)
                         (assert stats)
                         (inc! (car stats) (string->number (m 1)))
                         (inc! (cadr stats) (string->number (m 2))))]
                   [(#/Command being timed: \"([^ ]+) ([^\"]+)\"/ line)
                    => (lambda (m)
                         (assert stats)
                         (set! cmd (list (m 2) (m 1))))])
             (cond [(#/Elapsed \(wall clock\) time \(h:mm:ss or m:ss\): ((\d*):)?(\d+):(\d+(.\d+)?)/ line)
                    => (lambda (m)
                         (push! cmd (+ (* (+ (* (or (and (m 2) (string->number (m 2))) 0) 60)
                                             (string->number (m 3)))
                                          60)
                                       (string->number (m 4)))))]
                   [(#/Maximum resident set size \(kbytes\): (\d+)/ line)
                    => (lambda (m)
                         (push! cmd (* (string->number (m 1)) 1024)))]
                   [(#/Exit status: 0/ line)
                    (push! res (append script-run (reverse cmd) stats))
                    (set! cmd #f)])))
    (for-each (lambda(r)
                (write r)
                (newline))
              (reverse res)))
  0)
