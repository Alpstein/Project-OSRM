;;;
;;; routing web-service client
;;;
;;;   Copyright (c) 2013 Jens Thiele <karme@karme.de>
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  
(define-module routing-client
  (use srfi-1)
  (use gauche.collection)
  (use gauche.sequence)
  (use rfc.http)
  (use rfc.uri)
  (use rfc.json)
  (use sxml.tools)
  (use routing)
  (use util.list)
  (export routing-request
          routing-request-n
          routing-request-js
          routing-request-n-js))

(select-module routing-client)
;; (load "debug.scm")

;; todo: also in ...
(define-macro (debug-assert e)
  `(if (not ,e)
       (error "Debug assertion failed: " ,(x->string e))))

(define (pair->list x)
  (list (car x) (cdr x)))

(define (routing-http-request server request-uri method params reader)
  (let1 params (map pair->list params)
    (receive (status headers body)
        (case method
          [(post)
           (http-post server
                      request-uri
                      params)]
          [(get)
           (http-get server
                     (http-compose-query request-uri params))]
          [else
           (error "todo")])
      (reader body))))

;; todo: also in ...
;; taken from kahua (kahua-merge-headers)
(define (merge-headers headers . more-headers)
  (let outer ((headers headers)
              (more    more-headers))
    (if (null? more)
      headers
      (let inner ((headers headers)
                  (header (car more)))
        (if (null? header)
          (outer headers (cdr more))
          (inner (cons (car header) (alist-delete (caar header) headers))
                 (cdr header)))
        ))
    ))

(define (points->q points)
  (string-join (cons  (format #f "from:~a,~a" (cadr (car points)) (car (car points)))
                      (map
                       (lambda(p)
                         (format #f "to:~a,~a" (cadr p) (car p)))
                       (cdr points)))
               " "))

(define (routing-request-n epsg points . args)
  (let-optionals*
      args ((params '())
            (service '("mill" "/routingengine/getroute"))
            (method 'post))
    (let1 r (routing-http-request (car service)
                                  (cadr service)
                                  method
                                  (merge-headers params
                                                 `((q . ,(points->q points))
                                                   (epsg . ,epsg)
                                                   (format . sxml)))
                                  (lambda(body)
                                    (let1 r (guard (e
                                                    [else
                                                     (error "invalid answer: " body)])
                                                   (read-from-string body))
                                      (when (not (list? r))
                                        (error "body does not read as list" body))
                                      r)))
      (if (equal? (car r) 'error)
        (let1 e (read-from-string (sxml:attr r 'scm))
          (cond [(and (equal? (ref e 1) '<route-error>)
                      (symbol? (cadr (ref e 3))))
                 (error <route-error> :code (cadr (ref e 3)))]
                [(string? (ref e 1))
                 (error "upstream error: " (ref e 1))]
                [else
                 (debug-assert #f)]))
        r))))

(define (routing-request epsg from to . args)
  (apply routing-request-n (append (list epsg (list from to))
                                   args)))

(define (routing-request-n-js epsg points . args)
  (let-optionals*
      args ((params '())
            (service '("mill" "/routingengine/getroute")))
    (let1 json (routing-http-request (car service)
                                     (cadr service)
                                     'post
                                     (merge-headers params
                                                    `((q . ,(points->q points))
                                                      (epsg . ,epsg)
                                                      (format . "js")))
                                     (lambda(body)
                                       (guard (e
                                               [else
                                                (error "invalid answer: " body)])
                                              (parse-json-string body))))
      ;; todo: detect errors (even if jsfilter is used!)
      json)))

(define (routing-request-js epsg from to . args)
  (apply routing-request-n-js (append (list epsg (list from to))
                                      args)))
