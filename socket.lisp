(in-package #:network-engine)

(defvar *socket* nil)
(defvar *current-remote-host*)
(defvar *current-remote-port*)

(defun open-server-socket (host port)
  (assert (not *socket*))
  (setf *socket*
	(usocket:socket-connect nil
				nil
				:protocol :datagram
				:element-type '(unsigned-byte 8)
				:local-host host
				:local-port port)))

(defun open-client-socket (host port)
  (assert (not *socket*))
  (setf *socket* 
	(usocket:socket-connect host
				port
				:protocol :datagram
				:element-type '(unsigned-byte 8))))

(defun close-socket ()
  (assert *socket*)
  (usocket:socket-close *socket*)
  (setf *socket* nil))

;; (defun server-receive-packet ()
;;   (multiple-value-bind (buffer size in-host in-port)
;; 	  (usocket:socket-receive *socket* (make-array 32768 :element-type '(unsigned-byte 8) :fill-pointer t) nil)
;; 	(setf *current-remote-host* in-host)
;; 	(setf *current-remote-port* in-port)
;; 	buffer))

(defun receive-packets ()
  (loop until (not (usocket:wait-for-input network-engine:*socket* :timeout 0 :ready-only t)) do 
       (multiple-value-bind (buffer size in-host in-port)
	   (usocket:socket-receive *socket* (make-array 32768 :element-type '(unsigned-byte 8) :fill-pointer t) nil)
	 (setf *current-remote-host* in-host)
	 (setf *current-remote-port* in-port)
	 (let ((channel (lookup-channel-by-port in-port)))
	   (if channel
	       (receive-packet channel buffer)
	       (receive-packet (make-channel in-host in-port) buffer))))))
