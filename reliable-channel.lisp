(in-package #:network-engine)

(defclass reliable-channel (channel)
  ((local-sequence-number
    :initform 1
    :accessor sequence-number)
   (remote-sequence-number
    :initform 0
    :accessor remote-sequence-number)
   (acks
    :initform (list))
   (pending-ack-queue
    :initform (list)
    :accessor pending-ack-queue)
   (acked-queue
    :initform (list)
    :accessor acked-queue)
   (number-lost
    :initform 0
    :reader number-lost)
   (number-acked
    :initform 0
    :reader number-acked)
   (bandwidth-acked
    :initform 0
    :reader bandwidth-acked)))

(defun make-reliable-channel (remote-host remote-port)
  (make-instance 'reliable-channel :remote-host remote-host :remote-port remote-port))

(defmethod send-packet ((self reliable-channel) buffer)
  (with-slots (remote-host remote-port local-sequence-number sent-queue pending-ack-queue number-sent bandwidth-sent) self
    (let ((size (length buffer)))
      (usocket:socket-send *socket*
			   buffer
			   size
			   :host remote-host
			   :port remote-port)
      (assert (not (find local-sequence-number sent-queue :key #'message-sequence)))
      (assert (not (find local-sequence-number pending-ack-queue :key #'message-sequence)))
      (let ((msg (make-message :sequence local-sequence-number :time 0 :size size)))
	(setf sent-queue (append sent-queue (list msg)))
	(setf pending-ack-queue (append pending-ack-queue (list msg))))
      (incf number-sent)
      ;; (incf bandwidth-sent size)
      (incf local-sequence-number)
      (when (> local-sequence-number +max-sequence+)
	(setf local-sequence-number 0)))))

(defmethod receive-packet ((self reliable-channel) buffer)
  (with-slots (remote-sequence-number received-queue number-received bandwidth-received) self
    (userial:with-buffer buffer
      (userial:buffer-rewind)
      (userial:unserialize-let* (:uint32 sequence :uint32 ack :uint32 ack-bitfield)
	(incf number-received)
	;; (incf bandwidth-received (length buffer))
	(unless (find sequence received-queue :key #'message-sequence)
	  (setf received-queue (append received-queue (list (make-message :sequence sequence :time 0 :size (length buffer)))))	
	  (when (sequence-more-recent sequence remote-sequence-number)
	    (setf remote-sequence-number sequence))
	  (process-ack self ack ack-bitfield))))))

(defmethod generate-ack-bitfield ((self reliable-channel))
  (with-slots (remote-sequence-number received-queue) self
    (let ((ack-bitfield 0))
      (loop for msg in received-queue do
	   (unless (>= (message-sequence msg) remote-sequence-number) ;; TODO
	     (let ((bit-index (bit-index-for-sequence (message-sequence msg) remote-sequence-number)))
	       (when (<= bit-index 31)
		 (setf ack-bitfield (boole boole-ior ack-bitfield (ash 1 bit-index))))))) 
      ack-bitfield)))

(defmethod process-ack ((self reliable-channel) ack ack-bitfield)
  (with-slots (pending-ack-queue acked-queue acks number-acked rtt) self
    (when pending-ack-queue
      (loop for msg in pending-ack-queue do
	   (let ((acked-p nil))
	     (if (eq (message-sequence msg) ack)
		 (setf acked-p t)
		 (when (not (sequence-more-recent (message-sequence msg) ack))
		   (let ((bit-index (bit-index-for-sequence (message-sequence msg) ack)))
		     (when (and (<= bit-index 31) (not (= (boole boole-and (ash ack-bitfield (- bit-index)) 1) 0)))
		       (setf acked-p t)))))
	    (when acked-p
	      (setf rtt (+ rtt (* (- (message-time msg) rtt) 0.1)))
	      (setf acked-queue (append acked-queue (list msg))) ;; TODO sorted acked-queue insert msg
	      (setf acks (append acks (list (message-sequence msg))))
	      (incf number-acked)
	      (setf pending-ack-queue (remove msg pending-ack-queue))))))))

(defmethod channel-update ((self reliable-channel) delta-time)
  (with-slots (acks sent-queue received-queue pending-ack-queue acked-queue number-lost bandwidth-sent bandwidth-acked rtt-maximum) self
    (setf acks (list))
    
    ;;advance queue time
    (loop for msg in sent-queue do
	 (incf (message-time msg) delta-time))
    (loop for msg in received-queue do
	 (incf (message-time msg) delta-time))
    (loop for msg in pending-ack-queue do
	 (incf (message-time msg) delta-time))
    (loop for msg in acked-queue do
	 (incf (message-time msg) delta-time))
    
    ;; update queues
    (when received-queue
      (let* ((latest-sequence (message-sequence (first (last received-queue))))
	     (minimum-sequence (if (>= latest-sequence 34) (- latest-sequence 34) (- +max-sequence+ (- 34 latest-sequence)))))
	(setf received-queue (remove-if (lambda (x) (not (sequence-more-recent (message-sequence x) minimum-sequence))) received-queue))))

    (let ((epsilon 1))
      (setf sent-queue (remove-if (lambda (x) (> (message-time x) (+ rtt-maximum epsilon))) sent-queue))
      (setf acked-queue (remove-if (lambda (x) (> (message-time x) (- (* rtt-maximum 2) epsilon))) acked-queue))
      
      (loop for msg in pending-ack-queue do
	   (when (> (message-time msg) (+ rtt-maximum epsilon))
	     (setf pending-ack-queue (remove msg pending-ack-queue))
	     (incf number-lost)))
      ;; (setf pending-ack-queue (remove-if (lambda (x) (> (message-time x) (+ rtt-maximum epsilon))) pending-ack-queue))

      )

    ;; update stats
    (let ((sent-bytes-per-second 0)
	  (acked-packets-per-second 0)
	  (acked-bytes-per-second 0))
      
      (loop for msg in sent-queue do
	   (incf sent-bytes-per-second (message-size msg)))

      (loop for msg in acked-queue do
	   (when (>= (message-time msg) rtt-maximum)
	     (incf acked-packets-per-second)
	     (incf acked-bytes-per-second (message-size msg))))

      (setf bandwidth-sent (* (/ sent-bytes-per-second rtt-maximum) (/ 8 1000.0)))
      (setf bandwidth-acked (* (/ acked-bytes-per-second rtt-maximum) (/ 8 1000.0))))))
