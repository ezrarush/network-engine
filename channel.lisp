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
   (sent-queue
    :initform (list)
    :accessor sent-queue)
   (received-queue
    :initform (list)
    :accessor received-queue)
   (number-sent
    :initform 0
    :reader number-sent)
   (number-received
    :initform 0
    :reader number-received)
   (bandwidth-sent
    :initform 0
    :reader bandwidth-sent)
   (bandwidth-received
    :initform 0
    :reader bandwidth-received)
   (rtt
    :initform 0
    :reader rtt)
   (rtt-maximum
    :initform 1000)))

(defun make-channel (remote-host remote-port)
  (make-instance 'channel :remote-host remote-host :remote-port remote-port))

(defmethod initialize-instance :after ((self channel) &key)
  (add-channel-to-db self))

;; (defmethod send-packet ((self channel) buffer)
;;   (with-slots (remote-host remote-port number-sent sent-bandwidth) self
;;     (usocket:socket-send *socket*
;; 			 buffer
;; 			 (length buffer)
;; 			 :host remote-host
;; 			 :port remote-port)
;;     (incf number-sent)
;;     (incf sent-bandwidth (length buffer))))

;; (defmethod receive-packet ((self channel) buffer)
;;   (with-slots (received-queue number-received) self
;;     (push (make-message :buffer buffer
;; 			:time (sdl2:get-ticks)) 
;; 	  received-queue)
;;     (incf number-received)))
