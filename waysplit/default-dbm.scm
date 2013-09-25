(define-module default-dbm
  (extend dbm)
  (use dbm.tokyocabinet)
  (use dbm.gdbm)
  (export default-dbm-open
          default-dbm-class
          ))
(select-module default-dbm)

(define *default-class* <tcbdb>)
;;(define *default-class* <gdbm>)

(define (default-dbm-class)
  *default-class*)

(define (default-dbm-open . l)
  ;;(apply dbm-open (cons <tcbdb> l))
  (apply dbm-open (cons *default-class* l)))

