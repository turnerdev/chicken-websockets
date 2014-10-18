(import chicken scheme)
(use spiffy websockets)

(handle-not-found
 (lambda (path)
   (when (string= path "/web-socket")
         (with-websocket
          (lambda ()
            (send-message (string-append "you said: " (receive-message))))))))

(root-path ".")
(start-server port: 8080)
