;;;; pamixer.asd

(asdf:defsystem #:pamixer
  :description "PulseAudio volume and microphone control module for StumpWM"
  :author "Dmitrii Kosenkov"
  :license  "GPLv3"
  :version "0.1.1"
  :serial t
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "pamixer")))
