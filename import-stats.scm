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
                         (set! stats '((in-ways 0) (out-ways 0)))
                         (set! script-run `((date ,(m 1)))))]
                   [(#/splited \d+\/(\d+) ways into (\d+) ways/ line)
                    => (lambda (m)
                         ;;#?=(m)
                         (assert stats)
                         (inc! (cadar stats) (string->number (m 1)))
                         (inc! (cadadr stats) (string->number (m 2))))]
                   [(#/Command being timed: \"([^ ]+) ([^\"]+)\"/ line)
                    => (lambda (m)
                         (assert stats)
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
    (for-each (lambda(r)
                (write r)
                (newline))
              (reverse res)))
  0)
