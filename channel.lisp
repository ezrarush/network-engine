(in-package #:network-engine)

(defvar *channels* (make-hash-table))

(let ((next-id 0))
  (defun make-channel-id ()
    (incf next-id)))

(defun channel-id-in-use-p (id)
  (multiple-value-bind (object exists) (gethash id *channels*)
    (declare (ignore id))
    exists))

(defun lookup-channel-by-port (port)
  (let ((found nil))
    (loop for channel being the hash-value in *channels* do
	 (when (eq (remote-port channel) port)
	   (setf found channel)))
    found))

(defun lookup-channel-by-id (id)
  (multiple-value-bind (channel exists) (gethash id *channels*)
    (unless exists (error "No channel for id ~a." id))
    channel))

(defun add-channel-to-db (channel)
  (let ((id (channel-id channel)))
    (assert (not (channel-id-in-use-p id)))
    (setf (gethash id *channels*) channel)))

(defun remove-channel-from-db (channel)
  (let ((id (channel-id channel)))
    (assert (eql (lookup-channel-by-id id) channel))
    (remhash id *channels*)))

(defclass channel ()
  ((id 
    :type integer
    :initform (make-channel-id)
    :accessor channel-id)
   (remote-host
    :initarg :remote-host
    :accessor remote-host)
   (remote-port
    :initarg :remote-port
    :accessor remote-port)
   (local-sequence-number
    :initform 1
    :accessor sequence-number)
   (remote-sequence-number
    :initform 0
    :accessor remote-sequence-number)
   (sent-packets
    :initform (list)
    :accessor sent-packets)
   (received-packets
    :initform (list)
    :accessor received-packets)
   (pending-ack-packets
    :initform (list)
    :accessor pending-ack-packets)
   (acked-packets
    :initform (list)
    :accessor acked-packets)
   (number-sent
    :initform 0)
   (number-received
    :initform 0)
   (number-lost
    :initform 0)
   (number-acked
    :initform 0)
   (sent-bandwidth
    :initform 0)
   (acked-bandwidth
    :initform 0)
   (rtt
    :initform 0)))

(defmethod initialize-instance :after ((self channel) &key)
  (add-channel-to-db self))

(defmethod process-sent-packet ((self channel) time size)
  (with-slots (remote-host remote-port local-sequence-number sent-packets pending-ack-packets number-sent) self    
    (let ((data (make-packet-data :sequence local-sequence-number :time time :size size)))
      (setf sent-packets (append sent-packets (list data)))
      (setf pending-ack-packets (append pending-ack-packets (list data))))
    (incf number-sent)
    (incf local-sequence-number)))

(defmethod process-received-packet ((self channel) sequence ack ack-bitfield)
  (with-slots (number-received received-packets remote-sequence-number) self
    (let ((data (make-packet-data :sequence sequence :time (sdl2:get-ticks) :size nil)))
      (setf received-packets (append received-packets (list data))))
    (incf number-received)
    (when (> sequence remote-sequence-number)
      (setf remote-sequence-number sequence))
    (process-ack self ack ack-bitfield)))

(defmethod generate-ack-bitfield ((self channel))
  (with-slots (remote-sequence-number received-packets) self
    (let ((ack-bitfield 0))
      (loop for p-data in received-packets do
	   (unless (>= (packet-data-sequence p-data) remote-sequence-number)
	     (let ((bit-index (bit-index-for-sequence (packet-data-sequence p-data) remote-sequence-number)))
	       (when (<= bit-index 31)
		 (setf ack-bitfield (boole boole-ior ack-bitfield (ash 1 bit-index)))))))
      ack-bitfield)))

(defmethod process-ack ((self channel) ack ack-bitfield)
  (with-slots (pending-ack-packets acked-packets number-acked rtt) self
    (setf pending-ack-packets (remove-if (lambda (x) (or (eq (packet-data-sequence x) ack) (and (<= (packet-data-sequence x) ack) (<= (bit-index-for-sequence (packet-data-sequence x) ack) 31)))) pending-ack-packets))))

(defmethod update-metrics ((self channel))
  (with-slots (sent-packets received-packets pending-ack-packets acked-packets number-lost) self
    (delete-duplicates sent-packets)
    (delete-duplicates received-packets)
    (delete-duplicates pending-ack-packets)
    (delete-duplicates acked-packets)
    (let ((count 0)
	  (time (sdl2:get-ticks)))
      (loop for x in pending-ack-packets
	 counting (> (- time (packet-data-time x)) 1001) into count)
      (setf number-lost count)
      (setf pending-ack-packets (remove-if (lambda (x) (> (- time (packet-data-time x)) 1001)) pending-ack-packets))
      (setf sent-packets (remove-if (lambda (x) (> (- time (packet-data-time x)) 1001)) sent-packets))
      (setf acked-packets (remove-if (lambda (x) (> (- time (packet-data-time x)) 2001)) acked-packets)))
    (if received-packets
	(let* ((latest-sequence (packet-data-sequence (car (last received-packets))))
	       (minimum-sequence (- latest-sequence 34)))
	  (setf received-packets (remove-if (lambda (x) (<= (packet-data-sequence x) minimum-sequence)) received-packets))))))

(defun make-channel (remote-host remote-port)
  (make-instance 'channel :remote-host remote-host :remote-port remote-port))

(defstruct packet-data
  sequence
  time
  size)

(defun bit-index-for-sequence (sequence ack)
  ;; (assert (not (eq sequence ack)))
  ;; (assert (< sequence ack))
  (assert (>= ack 1))
  (assert (<= sequence (- ack 1)))
  (- ack 1 sequence))
