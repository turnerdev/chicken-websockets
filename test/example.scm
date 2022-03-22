(import spiffy websockets srfi-13)

(handle-not-found
 (lambda (path)
   (when (string= path "/web-socket")
         (with-websocket
          (lambda ()
	    (let loop ()
	      (send-message (string-append "you said: " (receive-message)))
	      (loop)))))))

(root-path ".")
(start-server port: 8080)
