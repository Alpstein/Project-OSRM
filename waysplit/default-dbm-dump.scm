#!/usr/bin/gosh -I.
;; -*- mode:scheme -*-
(use gauche.parseopt)
(use default-dbm)
(use util.match)
(use file.filter)

(define (main args)
  (let-args (cdr args) ([ofile "o=s" #f]
                        [else _ (usage)]
                        . args)
    (let1 class (default-dbm-class)
      (match args
        [(dbmname) (do-dump dbmname class (or ofile (current-output-port)))]
        [else (usage)]))
    0))

(define (usage)
  (print "Usage: gosh dbm/dump [-o outfile] dbmname")
  (exit 0))

(define (do-dump name class output)
  (let1 dbm (guard (e [else (exit 1 "couldn't open dbm database: ~a"
                                  (~ e'message))])
              (dbm-open class :path name :rw-mode :read))
    (file-filter
     (^(in out)
       ;; note: dbm-for-each broken in tokyocabinet atm
       (dbm-for-each dbm (^(k v) (write (cons k v) out) (newline out))))
     :output output :temporary-file output)
    (dbm-close dbm)))
