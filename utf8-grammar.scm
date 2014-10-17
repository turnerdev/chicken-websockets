(import chicken scheme)
(use srfi-4 srfi-13 srfi-14 comparse)


(define (ucs-range->char-set/inclusive lower upper)
  (ucs-range->char-set lower (add1 upper)))

(define utf8-tail
  (in (ucs-range->char-set/inclusive #x80 #xBF)))

(define utf8-1
  (in (ucs-range->char-set/inclusive #x00 #x7F)))

(define utf8-2
  (sequence
    (in (ucs-range->char-set/inclusive #xC2 #xDF))
    utf8-tail))

(define utf8-3
  (any-of
   (sequence
     (is #\xE0)
     (in (ucs-range->char-set/inclusive #xA0 #xBF))
     utf8-tail)
   (sequence
     (in (ucs-range->char-set/inclusive #xE1 #xEC))
     (repeated utf8-tail 2))
   (sequence
     (is #\xED)
     (in (ucs-range->char-set/inclusive #x80 #x9F))
     utf8-tail)
   (sequence
     (in (ucs-range->char-set/inclusive  #xEE #xEF))
     (repeated utf8-tail 2))))

(define utf8-4
  (any-of
   (sequence
     (is #\xF0)
     (in (ucs-range->char-set/inclusive #x90 #xBF))
     (repeated utf8-tail 2))
   (sequence
     (in (ucs-range->char-set/inclusive #xF1 #xF3))
     (repeated utf8-tail 3))
   (sequence
     (is #\xF4)
     (in (ucs-range->char-set/inclusive #x80 #x8F))
     (repeated utf8-tail 2))))

(define utf8-char
  (any-of
   utf8-1
   utf8-2
   utf8-3
   utf8-4))

(define utf8-string
  (followed-by (zero-or-more utf8-char) end-of-input))

;; (parse utf8-string (->parser-input "Hello-µ@ßöäüàá-UTF-8!!"))
;; (parse utf8-char (->parser-input #\a))

;; (define (valid-utf8? s)
;;   (let ((len (string-length s)))
;;     (let loop ((i 0))
;;       (if (= i len)
;;           #t
;;           (let ((r (parse utf8-char (->parser-input (->string (string-ref s i))))))
;;             (if r
;;                 (loop (+ i (length r)))
;;                 (string-ref s i)))))))
;; (valid-utf8? "Hello-µ@ßöäüàá-UTF-8!!")
;; (valid-utf8? "Hello")
;; (parse utf8-char (->parser-input (->string #\H)))

;; #\xC0
