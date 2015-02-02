;;;; package.lisp

(defpackage #:network-engine
  (:use #:cl)
  (:export
   
   ;;channel
   #:*channels*
   #:make-channel
   #:remote-host
   #:remote-port
   #:sequence-number
   #:remote-sequence-number
   #:process-sent-packet
   #:process-received-packet
   #:generate-ack-bitfield
   #:update-metrics
   #:lookup-channel-by-port))

