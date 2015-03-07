(in-package #:network-engine)

(defstruct message
  buffer
  sequence
  time
  size
  ttl)
