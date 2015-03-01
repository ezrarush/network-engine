;;;; package.lisp

(defpackage #:network-engine
  (:use #:cl)
  (:export

   ;;socket
   #:*socket*
   #:*current-remote-host*
   #:*current-remote-port*
   #:open-server-socket
   #:open-client-socket
   #:close-socket
   #:server-receive-packet
   #:receive-packets
   
   ;;channel
   #:*channels*
   #:make-channel
   #:remote-host
   #:remote-port
   #:sequence-number
   #:remote-sequence-number
   #:received-packets
   #:send-packet
   #:receive-packet
   #:process-sent-packet
   #:process-received-packet
   #:generate-ack-bitfield
   #:update-metrics
   #:lookup-channel-by-port
   
   ;;message
   #:message-buffer
   ))

