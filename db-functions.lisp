(in-package :shala-sys)

;;;; functions responsible for writing, reading, and other DB-related procedures are defined here.

(cl-mongo:db.use "shala-sys")

(defparameter *student-list* "students")

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
  (push pass (pass student))
  (setf (pass student)
        (sort-pass-dates (pass student))))

(define-method-with-update remove-pass (student pass)
  (setf (pass student)
        (sort-pass-dates (remove (first (pass-list-dates->yy-mm-dd (list pass)))
                                 (pass-list-dates->yy-mm-dd (pass student)) :test #'equalp))))

(define-method-with-update remove-last-pass (student)
  (setf (pass student) (rest (pass student))))

;;"Searches pass-list for pass and inserts a new pass in place of that with fields modified according to arg-list"
(define-method-with-update find-and-edit-pass (student pass pass-list new-pass)
  (multiple-value-bind (converted-pass converted-pass-list) (pass-and-pass-list-dates->yy-mm-dd pass pass-list)
    (when (member converted-pass converted-pass-list :test #'equalp) 
      (progn
        (setf (elt pass-list (position converted-pass converted-pass-list :test #'equalp))
              new-pass)
        (setf (pass student)
              ;; Sorts the pass dates non destructively then converts the date fields to local-time
              (pass-list-dates->timestamp (sort-pass-dates pass-list)))))))

(defun pass-and-pass-list-dates->yy-mm-dd (pass pass-list)
  "Converts the date fields of pass and pass-list to a string in yy-mm-dd format"
  (values (first (pass-list-dates->yy-mm-dd (list pass)))
          (pass-list-dates->yy-mm-dd pass-list)))
