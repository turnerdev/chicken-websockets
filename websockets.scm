(module websockets
  (
   ; parameters
   ping-interval close-timeout
   connection-timeout accept-connection
   drop-incoming-pings propagate-common-errors
   max-frame-size max-message-size

   ; high level API
   with-websocket with-concurrent-websocket
   send-message receive-message

   ; low level API
   ;; send-frame read-frame read-frame-payload
   ;; receive-fragments valid-utf8?
   ;; control-frame? upgrade-to-websocket
   ;; current-websocket unmask close-websocket
   ;; process-fragments

   ;; ; fragment
   ;; make-fragment fragment? fragment-payload fragment-length
   ;; fragment-masked? fragment-masking-key fragment-last?
   ;; fragment-optype
   )

(import chicken scheme data-structures extras ports posix foreign
        srfi-13 srfi-14 srfi-18)
(use srfi-1 srfi-4 spiffy intarweb uri-common base64 simple-sha1
     mailbox comparse)

(define-inline (neq? obj1 obj2) (not (eq? obj1 obj2)))

(define current-websocket (make-parameter #f))
(define ping-interval (make-parameter 15))
(define close-timeout (make-parameter 5))
(define connection-timeout (make-parameter 58)) ; a little grace period from 60s
(define accept-connection (make-parameter (lambda (origin) #t)))
(define drop-incoming-pings (make-parameter #t))
(define propagate-common-errors (make-parameter #f))
(define access-denied ; TODO test
  (make-parameter (lambda () (send-status 'forbidden "<h1>Access denied</h1>"))))

(define max-frame-size (make-parameter 1048576)) ; 1MiB
(define max-message-size
  (make-parameter 1048576 ; 1MiB
                  (lambda (v)
                    (if (> v 1073741823) ; max int size for unmask/utf8 check
                        (signal (make-property-condition 'out-of-range))
                        v))))

(define (make-websocket-exception . conditions)
  (apply make-composite-condition (append `(,(make-property-condition 'websocket))
                                          conditions)))

(define (make-protocol-violation-exception msg)
  (make-composite-condition (make-property-condition 'websocket)
                            (make-property-condition 'protocol-error 'msg msg)))

(define (opcode->optype op)
  (case op
    ((0) 'continuation)
    ((1) 'text)
    ((2) 'binary)
    ((8) 'connection-close)
    ((9) 'ping)
    ((10) 'pong)
    (else (signal (make-protocol-violation-exception "bad opcode")))))

(define (optype->opcode t)
  (case t
    ('continuation 0)
    ('text 1)
    ('binary 2)
    ('connection-close 8)
    ('ping 9)
    ('pong 10)
    (else (signal (make-websocket-exception
                   (make-property-condition 'invalid-optype))))))

(define (control-frame? optype)
  (or (eq? optype 'ping) (eq? optype 'pong) (eq? optype 'connection-close)))

(define-record-type websocket
  (make-websocket inbound-port outbound-port user-thread
                  send-mutex read-mutex last-message-timestamp
                  state send-mailbox read-mailbox concurrent)
  websocket?
  (inbound-port websocket-inbound-port)
  (outbound-port websocket-outbound-port)
  (user-thread websocket-user-thread)
  (send-mutex websocket-send-mutex)
  (read-mutex websocket-read-mutex)
  (last-message-timestamp websocket-last-message-timestamp
                          set-websocket-last-message-timestamp!)
  (state websocket-state set-websocket-state!)
  (send-mailbox websocket-send-mailbox)
  (read-mailbox websocket-read-mailbox)
  (concurrent websocket-concurrent?))

(define-record-type websocket-fragment
  (make-fragment payload length masked masking-key
                           fin optype)
  fragment?
  (payload fragment-payload)
  (length fragment-length)
  (masked fragment-masked? set-fragment-masked!)
  (masking-key fragment-masking-key)
  (fin fragment-last?)
  (optype fragment-optype))

(define (hex-string->string hexstr)
  ;; convert a string like "a745ff12" to a string
  (let ((result (make-string (/ (string-length hexstr) 2))))
    (let loop ((hexs (string->list hexstr))
               (i 0))
      (if (< (length hexs) 2)
          result
          (let ((ascii (string->number (string (car hexs) (cadr hexs)) 16)))
            (string-set! result i (integer->char ascii))
            (loop (cddr hexs)
                  (+ i 1)))))))


(define (send-frame ws optype data last-frame)
  ; TODO this sucks
  (when (u8vector? data) (set! data (blob->string (u8vector->blob/shared data))))
  (let* ((len (if (string? data) (string-length data) (u8vector-length data)))
         (frame-fin (if last-frame 1 0))
         (frame-rsv1 0)
         (frame-rsv2 0)
         (frame-rsv3 0)
         (frame-opcode (optype->opcode optype))
         (octet0 (bitwise-ior (arithmetic-shift frame-fin 7)
                              (arithmetic-shift frame-rsv1 6)
                              (arithmetic-shift frame-rsv2 5)
                              (arithmetic-shift frame-rsv3 4)
                              frame-opcode))

         (frame-masked 0)
         (frame-payload-length (cond ((< len 126) len)
                                     ((< len 65536) 126)
                                     (else 127)))
         (octet1 (bitwise-ior (arithmetic-shift frame-masked 7)
                              frame-payload-length))
         (outbound-port (websocket-outbound-port ws)))

    (write-u8vector (u8vector octet0 octet1) outbound-port)

    (write-u8vector
     (cond
      ((= frame-payload-length 126)
       (u8vector
        (arithmetic-shift (bitwise-and len 65280) -8)
        (bitwise-and len 255)))
      ((= frame-payload-length 127)
       (u8vector
        0 0 0 0
        (arithmetic-shift
         (bitwise-and len 4278190080) -24)
        (arithmetic-shift
         (bitwise-and len 16711680) -16)
        (arithmetic-shift
         (bitwise-and len 65280) -8)
        (bitwise-and len 255)))
      (else (u8vector)))
     outbound-port)

    (write-string data len outbound-port)
   (flush-output (response-port (current-response)))
    #t))

(define (send-message data #!optional (optype 'text) (ws (current-websocket)))
  ;; TODO break up large data into multiple frames?
  (optype->opcode optype) ; triggers error if invalid
  (dynamic-wind
      (lambda () (mutex-lock! (websocket-send-mutex ws)))
      (lambda () (send-frame ws optype data #t))
      (lambda () (mutex-unlock! (websocket-send-mutex ws)))))

(define (websocket-unmask-frame-payload payload len frame-masking-key)
  (define tmaskkey (make-u8vector 4 #f #t #t))
  (u8vector-set! tmaskkey 0 (vector-ref frame-masking-key 0))
  (u8vector-set! tmaskkey 1 (vector-ref frame-masking-key 1))
  (u8vector-set! tmaskkey 2 (vector-ref frame-masking-key 2))
  (u8vector-set! tmaskkey 3 (vector-ref frame-masking-key 3))

  ((foreign-lambda* void ((blob wsmaskkey) (int wslen) (scheme-pointer wsv))
"
    const unsigned char* maskkey2 = wsmaskkey;
    const unsigned int kd = *(unsigned int*)maskkey2;
    const unsigned char* __restrict kb = maskkey2;


    int i;
    for (i = wslen >> 2; i != 0; --i)
    {
        *((unsigned int*)wsv) ^= kd;
        wsv += 4;
    }

    const int rem = wslen & 3;
    for (i = 0; i < rem; ++i)
    {
        *((unsigned int*)wsv++) ^= kb[i];
    }
"
) (u8vector->blob/shared tmaskkey) len payload)
  payload)

(define (unmask fragment)
  (if (fragment-masked? fragment)
      (let ((r (websocket-unmask-frame-payload
                (fragment-payload fragment)
                (fragment-length fragment)
                (fragment-masking-key fragment))))
             (set-fragment-masked! fragment #f)
             r)
      (fragment-payload fragment)))

(define (read-frame-payload inbound-port frame-payload-length)
  (let ((masked-data (make-string frame-payload-length)))
    (read-string! frame-payload-length masked-data inbound-port)
    masked-data))

(define (read-frame total-size ws)
  (let* ((inbound-port (websocket-inbound-port ws))
         (b0 (read-byte inbound-port)))
    ; we don't support reserved bits yet
    (when (or (> (bitwise-and b0 64) 0)
              (> (bitwise-and b0 32) 0)
              (> (bitwise-and b0 16) 0))
          (signal (make-websocket-exception
                   (make-property-condition 'reserved-bits-not-supported)
                   (make-property-condition 'protocol-error))))
    (cond
     ((eof-object? b0) b0)
     (else
      (let* ((frame-fin (> (bitwise-and b0 128) 0))
             (frame-opcode (bitwise-and b0 15))
             (frame-optype (opcode->optype frame-opcode))
             ;; second byte
             (b1 (read-byte inbound-port))
             ; TODO die on unmasked frame?
             (frame-masked (> (bitwise-and b1 128) 0))
             (frame-payload-length (bitwise-and b1 127)))
        (cond ((= frame-payload-length 126)
               (let ((bl0 (read-byte inbound-port))
                     (bl1 (read-byte inbound-port)))
                 (set! frame-payload-length (+ (arithmetic-shift bl0 8) bl1))))
              ((= frame-payload-length 127)
               (define (shift i r)
                 (if (< i 0)
                     r
                     (shift (- i 1) (+ (arithmetic-shift (read-byte inbound-port) (* 8 i))
                                       r))))
               (set! frame-payload-length (shift 7 0))))
        (when (or (> frame-payload-length (max-frame-size))
                  (> (+ frame-payload-length total-size) (max-message-size)))
              (signal (make-websocket-exception
                       (make-property-condition 'message-too-large))))
        (let* ((frame-masking-key
                (if frame-masked
                    (let* ((fm0 (read-byte inbound-port))
                           (fm1 (read-byte inbound-port))
                           (fm2 (read-byte inbound-port))
                           (fm3 (read-byte inbound-port)))
                      (vector fm0 fm1 fm2 fm3))
                    #f)))
          (cond
           ((or (eq? frame-optype 'text) (eq? frame-optype 'binary)
                (eq? frame-optype 'continuation) (eq? frame-optype 'ping)
                (eq? frame-optype 'pong))
            (make-fragment
             (read-frame-payload inbound-port frame-payload-length)
             frame-payload-length frame-masked
             frame-masking-key frame-fin frame-optype))
           ((eq? frame-optype 'connection-close) ; TODO, same as above?
            (make-fragment
             (read-frame-payload inbound-port frame-payload-length)
             frame-payload-length frame-masked frame-masking-key
             frame-fin frame-optype))
           (else
            (signal (make-websocket-exception
                     (make-property-condition 'unhandled-optype
                                              'optype frame-optype)))))))))))

(include "utf8-grammar.scm")

(define (valid-utf8? s)
  (or (let ((len (string-length s)))
         ; Try to validate as an ascii string first. Its essentially
         ; free, doesn't generate garbage and is many, many times
         ; faster than the general purpose validator.
         (define-external ws_utlen int len)
         (define-external ws_uts scheme-pointer s)
         (= 1
            ((foreign-lambda* int ()
"
    if (ws_utlen > UINT_MAX) { return -1; }

    int i;
    for (i = ws_utlen; i != 0; --i)
    {
        if (*((unsigned char*)ws_uts++) > 127)
        {
            C_return(0);
        }
    }

    C_return(1);
"))))
      (parse utf8-string (->parser-input s))))

(define (close-code->integer s)
  (if (string-null? s)
      1000
      (+ (arithmetic-shift (char->integer (string-ref s 0)) 8)
         (char->integer (string-ref s 1)))))

(define (close-code-string->close-reason s)
  (let ((c (close-code->integer s)))
    (case c
      ((1000) 'normal)
      ((1001) 'going-away)
      ((1002) 'protocol-error)
      ((1003) 'unknown-data-type)
      ((1007) 'invalid-data)
      ((1008) 'violated-policy)
      ((1009) 'message-too-large)
      ((1010) 'extension-negotiation-failed)
      ((1011) 'unexpected-error)
      (else
       (if (and (>= c 3000) (< c 5000))
           'unknown
           'invalid-close-code)))))

(define (valid-close-code? s)
  (neq? 'invalid-close-code (close-code-string->close-reason s)))

(define (receive-fragments #!optional (ws (current-websocket)))
  (dynamic-wind
      (lambda () (mutex-lock! (websocket-read-mutex ws)))
      (lambda ()
        (if (or (eq? (websocket-state ws) 'closing)
                (eq? (websocket-state ws) 'closed)
                (eq? (websocket-state ws) 'error))
            (values #!eof #!eof)
            (let loop ((fragments '())
                       (first #t)
                       (type 'text)
                       (total-size 0))
              (let* ((fragment (read-frame total-size ws))
                     (optype (fragment-optype fragment))
                     (len (fragment-length fragment))
                     (last-frame (fragment-last? fragment)))
                (set-websocket-last-message-timestamp! ws (current-time))
                (cond
                 ((and (control-frame? optype) (> len 125))
                  (set-websocket-state! ws 'error)
                  (signal (make-protocol-violation-exception
                           "control frame bodies must be less than 126 octets")))

                 ; connection close
                 ((and (eq? optype 'connection-close) (= len 1))
                  (set-websocket-state! ws 'error)
                  (signal (make-protocol-violation-exception
                           "close frames must not have a length of 1")))
                 ((and (eq? optype 'connection-close)
                       (not (valid-close-code? (unmask fragment))))
                  (set-websocket-state! ws 'error)
                  (signal (make-protocol-violation-exception
                           (string-append
                            "invalid close code "
                            (number->string (close-code->integer (unmask fragment)))))))
                 ((eq? optype 'connection-close)
                  (set-websocket-state! ws 'closing)
                  (values `(,fragment) optype))

                 ; immediate response
                 ((and (eq? optype 'ping) last-frame (<= len 125))
                  (unless (drop-incoming-pings)
                          (send-message (unmask fragment) 'pong))
                  (loop fragments first type total-size))

                 ; protocol violation checks
                 ((or (and first (eq? optype 'continuation))
                      (and (not first) (neq? optype 'continuation)))
                  (set-websocket-state! ws 'error)
                  (signal (make-protocol-violation-exception
                           "continuation frame out-of-order")))
                 ((and (not last-frame) (control-frame? optype))
                  (set-websocket-state! ws 'error)
                  (signal (make-protocol-violation-exception
                           "control frames can't be fragmented")))

                 ((eq? optype 'pong)
                  (loop fragments first type total-size))

                 (else
                  (if last-frame
                      (values (cons fragment fragments) (if (null? fragments) optype type))
                      (loop (cons fragment fragments) #f
                            (if first optype type)
                            (+ total-size len)))))))))
      (lambda () (mutex-unlock! (websocket-read-mutex ws)))))

(define (process-fragments fragments optype #!optional (ws (current-websocket)))
  (let ((message-body (string-concatenate/shared
                       (reverse (map unmask fragments)))))
    (when (and (or (eq? optype 'text) (eq? optype 'connection-close))
               (not (valid-utf8?
                     (if (eq? optype 'text)
                         message-body
                         (if (> (string-length message-body) 2)
                             (substring message-body 2)
                             "")))))
          (set-websocket-state! ws 'error)
          (signal (make-websocket-exception
                   (make-property-condition
                    'invalid-data 'msg "invalid UTF-8"))))
    (values message-body optype)))

(define (receive-message #!optional (ws (current-websocket)))
  (if (websocket-concurrent? ws)
      (let ((msg (mailbox-receive! (websocket-read-mailbox ws))))
        (values (car msg) (cdr msg)))
      (receive (fragments optype) (receive-fragments ws)
               (if (eof-object? fragments)
                   (values #!eof optype)
                   (process-fragments fragments optype)))))

; TODO does #!optional and #!key work together?
(define (close-websocket #!optional (ws (current-websocket))
                         #!key (close-reason 'normal) (data (make-u8vector 0)))
  (define invalid-close-reason #f)
  (define (close-reason->close-code reason)
    (case reason
      ('normal 1000)
      ('going-away 1001)
      ('protocol-error 1002)
      ('unknown-data-type 1003)
      ('invalid-data 1007)
      ('violated-policy 1008)
      ('message-too-large 1009)
      ('unexpected-error 1011)
      (else (set! invalid-close-reason reason)
            (close-reason->close-code 'unexpected-error))))

  ; Use thread timeout to handle the close-timeout
  (let ((close-thread
         (make-thread
          (lambda ()
            (if (eq? (websocket-state ws) 'open)
                (begin
                  (set-websocket-state! ws 'closed)
                  (send-frame ws 'connection-close
                                        (u8vector 3 (close-reason->close-code close-reason))
                                        #t)
                  (let loop ()
                    (receive (data type) (receive-message ws)
                             (if (eq? type 'connection-close)
                                 (unless (valid-utf8? data)
                                         (set! close-reason 'invalid-data))
                                 (loop)))))
                (begin
                  (send-frame ws 'connection-close
                                        (u8vector 3 (close-reason->close-code close-reason))
                                        #t))))
          "close timeout thread")))
    (thread-start! close-thread)
    (if (> (close-timeout) 0)
        (unless (thread-join! close-thread (close-timeout) #f)
                ; TODO actually signal error?
                ;; (thread-signal! (websocket-user-thread (current-websocket))
                ;;                 (make-websocket-exception
                ;;                  (make-property-condition 'close-timeout)))
                )
        (thread-join! close-thread))))


(define (sha1-sum in-bv)
  (hex-string->string (string->sha1sum in-bv)))

(define (websocket-compute-handshake client-key)
  (let* ((key-and-magic
          (string-append client-key "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"))
         (key-and-magic-sha1 (sha1-sum key-and-magic)))
    (base64-encode key-and-magic-sha1)))

(define (sec-websocket-accept-unparser header-contents)
  (map (lambda (header-content)
         (car (vector-ref header-content 0)))
       header-contents))

(header-unparsers
 (alist-update! 'sec-websocket-accept
                sec-websocket-accept-unparser
                (header-unparsers)))

(define (websocket-accept #!optional (concurrent #f))
  (let* ((user-thread (current-thread))
         (headers (request-headers (current-request)))
         (client-key (header-value 'sec-websocket-key headers))
         (ws-handshake (websocket-compute-handshake client-key))
         (ws (make-websocket
              (request-port (current-request))
              (response-port (current-response))
              user-thread
              (make-mutex "send")
              (make-mutex "read")
              (current-time)
              'open               ; websocket state
              (make-mailbox "send")
              (make-mailbox "read")
              concurrent))
         (ping-thread
          (make-thread
           (lambda ()
             (let loop ()
               (thread-sleep! (ping-interval))
               (when (eq? (websocket-state ws) 'open)
                     (send-message "" 'ping ws)
                     (loop))))
           "ping thread")))

    ; make sure the request meets the spec for websockets
    (cond ((not (and (member 'upgrade (header-values 'connection headers))
                     (string-ci= (car (header-value 'upgrade headers '(""))) "websocket")))
           (signal (make-websocket-exception
                    (make-property-condition 'missing-upgrade-header))))
          ((not (string= (header-value 'sec-websocket-version headers "") "13"))
           (with-headers ; TODO test
            `((sec-websocket-version "13"))
            (lambda () (send-status 'upgrade-required))))
          ((not ((accept-connection) (header-value 'origin headers "")))
           ((access-denied))))

    (with-headers
     `((upgrade ("WebSocket" . #f))
       (connection (upgrade . #t))
       (sec-websocket-accept (,ws-handshake . #t)))
     (lambda ()
       (send-response status: 'switching-protocols)))
    (flush-output (response-port (current-response)))

    ; connection timeout thread
    (when (> (connection-timeout) 0)
          (thread-start!
           (lambda ()
             (let loop ()
               (let ((t (websocket-last-message-timestamp ws)))
                  ; Add one to attempt to alleviate checking the timestamp
                  ; right before when the timeout should happen.
                 (thread-sleep! (+ 1 (connection-timeout)))
                 (when (eq? (websocket-state ws) 'open)
                       (if (< (- (time->seconds (current-time))
                                 (time->seconds (websocket-last-message-timestamp ws)))
                              (connection-timeout))
                           (loop)
                           (begin (thread-signal!
                                   (websocket-user-thread ws)
                                   (make-websocket-exception
                                    (make-property-condition 'connection-timeout)))
                                  (close-websocket ws close-reason: 'going-away)))))))))

    (when (> (ping-interval) 0)
          (thread-start! ping-thread))

    ws))

(define (with-websocket proc #!optional (concurrent #f))
  (define (handle-error close-reason exn)
    (set-websocket-state! (current-websocket) 'closing)
    (close-websocket (current-websocket) close-reason: close-reason)
    (unless (port-closed? (request-port (current-request)))
            (close-input-port (request-port (current-request))))
    (unless (port-closed? (response-port (current-response)))
            (close-output-port (response-port (current-response))))
    (when (propagate-common-errors)
          (signal exn)))
  (parameterize
   ((current-websocket (websocket-accept concurrent)))
   (condition-case
    (begin (proc)
           (close-websocket)
           (close-input-port (request-port (current-request)))
           (close-output-port (response-port (current-response))))
    (exn (websocket protocol-error) (handle-error 'protocol-error exn))
    (exn (websocket invalid-data) (handle-error 'invalid-data exn))
    (exn (websocket connection-timeout) (handle-error 'going-away exn))
    (exn (websocket message-too-large) (handle-error 'message-too-large exn))
    (exn () (handle-error 'unexpected-error exn)))))

(define (with-concurrent-websocket proc)
  (let ((parent-thread (current-thread)))
    (with-websocket
     (lambda ()
       (thread-start!
        (lambda ()
          (handle-exceptions
           exn
           (thread-signal! parent-thread exn)
           (let loop ()
             (receive (fragments optype) (receive-fragments)
                      (unless (eof-object? fragments)
                              (thread-start!
                               (lambda ()
                                 (handle-exceptions
                                  exn
                                  (thread-signal! parent-thread exn)
                                  (mailbox-send!
                                   (websocket-read-mailbox (current-websocket))
                                   (receive (msg-body optype)
                                            (process-fragments fragments optype)
                                            `(,msg-body . ,optype))))))
                              (loop)))))))
       (proc))
     #t)))

(define (upgrade-to-websocket #!optional (concurrent #f))
  (websocket-accept concurrent))

)
