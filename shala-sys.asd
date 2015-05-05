;;;; shala-sys.asd

(asdf:defsystem #:shala-sys
  :description "Shala book-keeping system"
  :author "Mozart Reina"
  :license "BSD license"
  :serial t
  :depends-on (#:parenscript
               #:hunchentoot
               #:cl-mongo
               #:local-time
               #:cl-json
               #:split-sequence)
  :components ((:file "package")
               (:file "shala-sys")
               (:file "shala-sys-tests")
               (:file "time")
               (:file "db-funcs")
               (:file "db-funcs-utils")
               (:file "views")
               (:file "queries")
               (:file "utils")))

