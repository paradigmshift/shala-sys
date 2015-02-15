(in-package :shala-sys)

;;;; Functions responsible for writing, reading, and other DB-related procedures are defined here.

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

;;"Searches pass-list for pass and inserts a new pass in place of that with fields modified according to arg-list"
(define-method-with-update find-and-edit-pass (student pass pass-list new-pass)
  (multiple-value-bind (converted-pass converted-pass-list) (convert-pass-and-pass-list-dates pass pass-list)
    (when (member converted-pass converted-pass-list :test #'equalp) 
      (progn
        (setf (elt pass-list (position converted-pass converted-pass-list :test #'equalp))
              new-pass)
        (setf (pass student)
              ;; Sorts the pass dates non destructively then converts the date fields to local-time
              (pass-list-dates->timestamp (sort-pass-dates pass-list)))))))

(defun students ()
  "Retrieves the list of students from the DB, sorted"
  (map 'list #'(lambda (doc)
                 (doc->student doc))
       (docs (iter (db.sort *student-list* :all :field "name")))))

(defun student-from-name (name)
  (let ((found-students (docs (db.find *student-list* ($ "name" name)))))
    (when found-students
      (doc->student (first found-students)))))

(defun register-student (student)
  (db.insert *student-list* (student->doc student)))

(defun convert-pass-and-pass-list-dates (pass pass-list)
  "Converts the date fields of pass and pass-list to a string in yy-mm-dd format"
  (let ((converted-pass (copy-list pass))
        (converted-pass-list (copy-tree pass-list)))
    (pass-list-dates->yy-mm-dd (append (list converted-pass) converted-pass-list))
    (values converted-pass converted-pass-list)))

(defun pass-list-dates->yy-mm-dd (pass-list)
  "Converts the date fields of pass-list to a string in yy-mm-dd format"
  (map 'list #'(lambda (pass)
                 (setf (getf pass :date)
                       (print-year-month-day (get-date pass))))
       pass-list)
  pass-list)

(defun pass-list-dates->timestamp (pass-list)
  "Converts the date fields of pass-list from yy-mm-dd to a local-time:timestamp"
  (map 'list #'(lambda (pass)
                 (when (not (typep (getf pass :date) 'local-time:timestamp))
                   (setf (getf pass :date)
                         (print-year-month-day->timestamp (get-date pass)))))
       pass-list)
  pass-list)

(defun sort-pass-dates (pass-list)
  "Sorts the list of passes according to dates in descending order. It first transforms the dates from
   yy-mm-dd to local-time, then from local-time to unix, sorts, then back again to local-time"
  (let ((pass-list-copy (copy-tree pass-list)))
    (let ((pass-list-copy
           (pass-list-dates-> (pass-list-dates->timestamp pass-list-copy)
                              :fn #'local->unix)))
      (sort pass-list-copy #'> :key #'get-date)
      (pass-list-dates-> pass-list-copy :fn #'unix->local))))

(defun pass-list-dates-> (pass-list &key fn)
  "Converts the date fields of a list of passes based on the function passed to fn. For example, if
   if unix->local is passed in, it will try to convert the date field from unix time to local-time."
  (let ((pass-list-copy (copy-tree pass-list)))
    (map 'list #'(lambda (pass)
                   (setf (getf pass :date)
                         (funcall fn (get-date pass))))
         pass-list-copy)
    pass-list-copy))

(defun get-date (plist)
  "Retrieve the date field from the plist"
  (getf plist :date))
