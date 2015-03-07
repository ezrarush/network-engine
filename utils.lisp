(in-package #:network-engine)

(defconstant +max-sequence+ 214483600)

(defun sequence-more-recent (a b)
  (or (and (> a b) (<= (- a b) (/ +max-sequence+ 2))) (and (< a b) (> (- b a) (/ +max-sequence+ 2)))))

(defun bit-index-for-sequence (sequence ack)
  (assert (not (= sequence ack)))
  (assert (not (sequence-more-recent sequence ack)))
  (if (> sequence ack)
      (progn
	(break) 
	(assert (< ack 33))
	(assert (>= +max-sequence+ sequence))
	(+ ack (- +max-sequence+ - sequence)))
      (progn
	(assert (>= ack 1))
	(assert (<= sequence (- ack 1)))
	(- ack 1 sequence))))

