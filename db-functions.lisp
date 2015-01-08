(in-package :shala-sys)

(cl-mongo:db.use "shala-sys")

(defparameter *student-list* "students")

(defun student->doc (student)
  "Creates MongoDB document from student object"
  (with-slots (name email pass drop-in attendance) student
    ($ ($ "name" name)
       ($ "email" email)
       ($ "pass" (convert-passes pass))
       ($ "drop-in" (convert-drop-in drop-in))
       ($ "attendance" attendance))))

(defun doc->student (doc)
  "Creates student object from MongoDB document"
  (make-instance 'student :name (get-element "name" doc)
                 :email (get-element "email" doc)
                 :pass (reconvert-passes (get-element "pass" doc)) ;pass plist is converted to string when saved in the database
                 :drop-in (reconvert-drop-in (get-element "drop-in" doc))
                 :attendance (get-element "attendance" doc)))


;; :after methods for persisting the changes into the database
;; (defmacro after-method (fn-name &rest params)
;;   (let ((student (first params)))
;;     `(defmethod ,fn-name :after ,params
;;                 (let ((student-doc (student->doc ,student)))
;;                   (db.update *student-list* ($ "name" (name ,student))
;;                              student-doc)))))

;; (defmethod new-pass (student pass)
;;   "Push pass to student's records"
;;   (push pass (pass student)))

;; (defmethod remove-last-pass (student)
;;   (setf (pass student) (rest (pass student))))

;; (defmethod new-drop-in (student drop-in)
;;   "Push drop-in to student's records"
;;   (push drop-in (drop-in student)))

(defmacro define-method-with-update (method-name (&rest args) &body body)
  "Defines a method and an :after method for persisting changes by the method to the database"
  (let ((student (first args)))
    `(progn 
       (defmethod ,method-name ,args
         ,@body)
       (defmethod ,method-name :after ,args
                  (let ((student-doc (student->doc ,student)))
                    (db.update *student-list* ($ "name" (name ,student))
                               student-doc))))))

(define-method-with-update new-drop-in (student drop-in)
  (push drop-in (drop-in student)))

(define-method-with-update new-pass (student pass)
  (push pass (pass student)))

(define-method-with-update remove-last-pass (student)
  (setf (pass student) (rest (pass student))))

;; (after-method remove-last-pass student)
;; (after-method new-pass student pass)
;; (after-method new-drop-in student drop-in)

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
