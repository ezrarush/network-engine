;;;; network-engine.asd

(asdf:defsystem #:network-engine
  :description "UDP network engine"
  :author "Ezra Rush <rushwest@gmail.com>"
  :license "The MIT License (MIT) Copyright (c) 2015 Ezra Rush"
  :serial t
  :depends-on (#:usocket)
  :components ((:file "package")
	       (:file "message")
	       (:file "socket")
	       (:file "channel")))

