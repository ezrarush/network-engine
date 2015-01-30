;;;; network-engine.asd

(asdf:defsystem #:network-engine
  :description "UDP network engine"
  :author "Ezra Rush <rushwest@gmail.com>"
  :license "The MIT License (MIT) Copyright (c) 2015 Ezra Rush"
  :serial t
  :components ((:file "package")
	       (:file "channel")))

