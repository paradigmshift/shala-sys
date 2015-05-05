;;;; shala-sys.lisp

(in-package #:shala-sys)



;;; "shala-sys" goes here. Hacks and glory await!
(defvar *students-today* '() "students present today")

(defparameter *type-map* '((M . 30)
                           (E . 30)
                           (W . 7)))

(defclass student ()
  ((name :initarg :name
         :accessor name)
   (email :initarg :email
          :accessor email)
   (pass :initarg :pass
         :accessor pass
         :initform nil)
   (drop-in :initarg :drop-in
            :accessor drop-in
            :initform nil)
   (attendance :initarg :attendance
               :accessor attendance
               :initform nil)))

(defclass expense ()
  ((date :initarg :date
         :accessor date)
   (comment :initarg :comment
            :accessor comment)
   (amount :initarg :amount
           :accessor amount)))

(defmethod print-object ((object student) stream)
  "Make the student class information readable from the REPL"
  (print-unreadable-object (object stream :type t)
    (with-slots (name email pass drop-in) object
      (format stream "name: ~s email: ~s pass: ~s drop-in :~s" name email pass drop-in))))

(defmethod print-object ((object expense) stream)
  "Make the expense class information readable from the REPL"
  (print-unreadable-object (object stream :type t)
    (with-slots (date comment amount) object
      (format stream "date: ~s comment: ~s amount: ~s" date comment amount))))

(defun remove-nil (list)
  "Remove nil elements from list"
  (remove-if #'null list))

(defun sum (list)
  (reduce #'+ list))

(defun remove-from-today (name)
  "Remove student from today's list"
  (setf *students-today* (remove-if #'(lambda (student)
                                        (equalp (name student) name))
                                    *students-today*)))
