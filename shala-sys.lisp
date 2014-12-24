;;;; shala-sys.lisp

(in-package #:shala-sys)

;;; "shala-sys" goes here. Hacks and glory await!

(defvar *students* '() "all students past and present")

(defvar *students-today* '() "students present today")

(defparameter *type-map* '((m . 30)
                           (e . 30)
                           (w . 7)))

(defclass student ()
  ((name :initarg :name
         :accessor name)
   (email :initarg :email
          :accessor email)
   (pass :accessor pass
         :initform nil)
   (drop-in :accessor drop-in
            :initform nil)
   (attendance :accessor attendance
               :initform nil)))

(defmethod print-object ((object student) stream)
  "Make the student class information readable from the REPL"
  (print-unreadable-object (object stream :type t)
    (with-slots (name email pass) object
      (format stream "name: ~s email: ~s pass: ~s" name email pass))))

(defun new-student (&key name email)
  "Instantiate new student"
  (make-instance 'student :name name :email email))

(defun student-from-name (name)
  "Retrieve student from name"
  (first (remove-if-not #'(lambda (student)
                            (equalp (name student) name))
                        *students*)))

(defun student-from-name-fuzzy (name)
  "Fuzzy version of student-from-name"
  (remove-if-not #'(lambda (student)
                     (search name (name student)))
                 *students*))

(defun register-student (student)
  "Push student to student list"
  (push student *students*))

(defun make-pass (&key type start-date amt)
  "Return a list with the pass information"
  (list :type type :start-date start-date :amt amt))

;; (defun new-pass (student pass)
;;   "Push new pass to student"
;;   (push pass (pass student)))

(defmethod new-pass (student pass)
  (push pass (pass student)))

(defun pass-of (student)
  "Retrieve latest pass from student"
  (first (pass student)))

(defun pass-p (student)
  "Test if student has pass, predicate"
  (not (null (pass student))))

(defun get-start-date (pass)
  "Retrieve the start-date from the pass"
  (getf pass :start-date))

(defun get-type (pass)
  "Retrieve type from pass, m - morning, e - evening, w - 1 week"
  (getf pass :type))

(defun pass-info (pass)
  "Retrieve start-date and type of pass"
  (values (get-start-date pass) (get-type pass)))

;; (defun validate-pass (student) 
;;   "Validate pass according to its type, validity based on *type-map*"
;;   (multiple-value-bind (start-date type) (pass-info (pass-of student))
;;     (local-time:timestamp>
;;      (cond ((or (equalp type 'm) (equalp type 'e))
;;             (local-time:timestamp+ start-date (rest (assoc 'm *type-map*)) :day))
;;            (t
;;             (local-time:timestamp+ start-date (rest (assoc 'w *type-map*)) :day)))
;;      (local-time:now))))

(defun expired-p (type startdate)
  "checks to see if the pass is expired based on type and startdate, compared with date today, time minimized to 0:00"
  (let ((duration (rest (assoc type *type-map*))))
    (local-time:timestamp< (local-time:adjust-timestamp startdate (offset :day duration))
                           (local-time:timestamp-minimize-part (time-now) :hour))))
(defun validate-pass (student) 
  "Validate pass according to its type, validity based on *type-map*"
  (multiple-value-bind (start-date type) (pass-info (pass-of student))
    (not (expired-p type start-date))))

(defun remove-from-today (name)
  "Remove student from today's list"
  (setf *students-today* (remove-if #'(lambda (student)
                                        (equalp (name student) name))
                                    *students-today*)))

;;;; HTML
(defun start-server (port)
  (start (make-instance 'hunchentoot:easy-acceptor :port port)))

(defun serve-static-file (fname uri)
  "Push the filename's URI to the dispatch table"
  (push (create-static-file-dispatcher-and-handler fname uri)
        *dispatch-table*))

(defmacro standard-page ((&key title script) &body body)
  `(with-html-output-to-string
       (*standard-output* nil :prologue t :indent t)
     (:html :lang "en"
            (:head
             (:meta :charset "utf-8")
             (:meta :name "viewport" :content "width=device-width, initial-scale=1") ;mobile friendly

             (:title ,title)
             (:link :type "text/css"
                    :rel "stylesheet"
                    :href "/style.css")
             ,(when script
                `(:script :type "text/javascript"
                          (str, script))))
            (:body
             (:div :id "header" ;page header
                   )
             ,@body))))

(define-easy-handler (main :uri "/main") ()
  (standard-page (:title "Ashtanga Yoga Osaka")
    (:h1 "Students today")
    (:table :id "maintable"
     (:caption "Students Today")
     (:col)
     (:thead
      (:tr (:th "Name")
           (:th "Pass")))
     (:tbody ;Displays list of students that attended today
      (dolist (student *students-today*)
        (htm
         (:tr
          (:td (:form (:button :name "name"
                               :type "submit"
                               :formmethod "POST"
                               :formaction "/remove-today"
                               :value (format nil "~A" (name student))
                               "X")) ; Button to remove from today's list
               (fmt "~A" (name student)))
          (:td (fmt "~A" (get-type (first (pass student))))))))))
    (:a :href "student-list" "Total list of students")
    (:a :href "add-student-to-class" "+ student")))

;; Remove from today's list
(define-easy-handler (remove-today :uri "/remove-today") (name) 
  (standard-page (:title "Ashtanga Yoga Osaka")
    (remove-from-today name)
    (redirect "/main")))

(define-easy-handler (student-list :uri "/student-list") ()
  (standard-page (:title "Ashtanga Yoga Osaka | Student List")
    (:h1 "Student List")
    (:div :id "student-table"
          (:ol (dolist (student *students*)
                 (htm
                  (:li (fmt "Name: ~A Email: ~A Latest Pass: ~A" (escape-string (name student))
                            (email student)
                            (first (pass student))))))))
    (:a :href "main" "Back to Main")))

;; Add student to today's list
(define-easy-handler (add-student-to-class :uri "/add-student-to-class") ()
  (standard-page (:title "Ashtanga Yoga Osaka | Add student to class")
    (:h1 "Add student to class")
    (:form :action "/validate-and-add" :method "post" :id "validate-student"
           (:p "Name of the student" (:input :type "text" :name "name" :class "txt"))
           (:p (:input :type "submit" :value "Validate" :class "btn")))))

;; Validate the pass and add the student to today's class
(define-easy-handler (validate-and-add :uri "/validate-and-add") (name)
  (let ((validate-p (validate-pass (student-from-name name))))
    (with-html-output-to-string
        (*standard-output* nil :prologue t :indent t)
      (:html
       (cond ((equal validate-p t) (progn (push (student-from-name name) *students-today*)
                                          (redirect "/main")))
             (t (htm (:p "Pass not valid")))))))) 

;;;; time component - may change
(ql:quickload "local-time")

(defun time-now ()
  (local-time:now))

(defun add-months (months timestamp)
  (local-time:adjust-timestamp timestamp (offset :month months)))

(defun add-days (days timestamp)
  (local-time:adjust-timestamp timestamp (offset :day days)))

(defun print-month (timestamp)
  (local-time:format-timestring nil timestamp :format '(:short-month)))

(defun print-day (timestamp)
  (local-time:format-timestring nil timestamp :format '(:day)))
