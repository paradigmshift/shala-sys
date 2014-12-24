;;;; shala-sys.asd

(asdf:defsystem #:shala-sys
  :description "Shala book-keeping system"
  :author "Mozart Reina"
  :license "BSD license"
  :serial t
  :depends-on (#:parenscript
               #:hunchentoot
               #:cl-mongo
               #:local-time)
  :components ((:file "package")
               (:file "shala-sys")
               (:file "shala-sys-tests")))

