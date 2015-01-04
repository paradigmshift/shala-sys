(in-package :shala-sys)

(cl-mongo:db.use "shala-sys")

(defparameter *student-list* "students")

(defun student->doc (student)
  (with-slots (name email pass drop-in attendance) student
    ($ ($ "name" name)
       ($ "email" email)
       ($ "pass" (convert-passes pass))
       ($ "drop-in" (convert-drop-in drop-in))
       ($ "attendance" attendance))))

(defun doc->student (doc)
  (make-instance 'student :name (get-element "name" doc)
                 :email (get-element "email" doc)
                 :pass (reconvert-passes (get-element "pass" doc)) ;pass plist is converted to string when saved in the database
                 :drop-in (reconvert-drop-in (get-element "drop-in" doc))
                 :attendance (get-element "attendance" doc)))


;; :after methods for persisting the changes into the database
(defmacro after-method (fn-name &rest params)
  (let ((student (first params)))
    `(defmethod ,fn-name :after ,params
                (let ((student-doc (student->doc ,student)))
                  (db.update *student-list* ($ "name" (name ,student))
                             student-doc)))))

(after-method remove-last-pass student)
(after-method new-pass student pass)
(after-method new-drop-in student drop-in)

;; (defmethod new-drop-in :after (student date amt)
;;   (let ((student-doc (student->doc student)))
;;     (db.update *student-list* ($ "name" (name student))
;;                student-doc)))

;; (defmethod remove-last-pass :after (student)
;;   (let ((student-doc (student->doc student)))
;;     (db.update *student-list* ($ "name" (name student))
;;                student-doc)))

;; (defmethod new-pass :after (student pass)
;;   (let ((student-doc (student->doc student)))
;;     (db.update *student-list* ($ "name" (name student))
;;                student-doc)))

(defun students ()
  (map 'list #'(lambda (doc)
                 (doc->student doc))
       (docs (iter (db.find *student-list* :all)))))

(defun student-from-name (name)
  (let ((found-students (docs (db.find *student-list* ($ "name" name)))))
    (when found-students
      (doc->student (first found-students)))))

(defun register-student (student)
  (db.insert *student-list* (student->doc student)))

