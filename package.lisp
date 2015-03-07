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
   #:receive-packets
   #:receive-reliable-packets
   
   ;;channel
   #:*channels*
   #:make-channel
   #:remote-host
   #:remote-port
   #:sequence-number
   #:remote-sequence-number
   #:rtt
   #:number-sent
   #:number-acked
   #:bandwidth-sent
   #:bandwidth-acked
   #:number-lost
   #:number-acked
   #:send-packet
   #:receive-packet
   #:process-sent-packet
   #:process-received-packet
   #:generate-ack-bitfield
   #:channel-update
   #:lookup-channel-by-port

   ;;reliable-channel
   #:make-reliable-channel
   
   ;;message
   #:message-buffer
   ))

