(import chicken scheme posix)
(use spiffy websockets)

(ping-interval 0)
(drop-incoming-pings #f)
(propagate-common-errors #f)
(max-message-size 20971520)
(max-frame-size 20971520)

(handle-not-found
  (lambda (path)
    (with-websocket
         (lambda ()
           (let loop ()
             (receive (data type) (receive-message)
                      (unless (eq? type 'connection-close)
                              (send-message data type)
                              (loop))))))))

(debug-log (current-output-port))
(root-path ".")
(server-port 8080)
(start-server)


