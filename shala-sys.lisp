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
    (with-slots (name email pass drop-in) object
      (format stream "name: ~s email: ~s pass: ~s drop-in :~s" name email pass drop-in))))

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

(defmethod new-pass (student pass)
  "Push pass to student's records"
  (push pass (pass student)))

(defmethod new-drop-in (student date amt)
  "Push drop-in to student's records"
  (push (list :date date :amt amt) (drop-in student)))

(defun pass-of (student)
  "Retrieve latest pass from student"
  (first (pass student)))

(defun pass-p (student)
  "Test if student has pass, predicate"
  (not (null (pass student))))

(defun get-start-date (pass)
  "Retrieve the start-date from the pass"
  (getf pass :start-date))

(defun print-month-day (timestamp)
  (format nil "~A ~A" (print-month timestamp) (print-day timestamp)))

(defun get-type (pass)
  "Retrieve type from pass, m - morning, e - evening, w - 1 week"
  (getf pass :type))

(defun pass-info (pass)
  "Retrieve start-date and type of pass"
  (values (get-start-date pass) (get-type pass)))

(defun expired-p (type startdate)
  "checks to see if the pass is expired based on type and startdate, compared with date today, time minimized to 0:00"
  (let ((duration (rest (assoc type *type-map*))))
    (local-time:timestamp< (local-time:adjust-timestamp startdate (offset :day duration))
                           (local-time:timestamp-minimize-part (time-now) :hour))))

(defun validate-pass (student) 
  "Validate pass according to its type, validity based on *type-map*"
  (when (not (null (pass-p student)))
    (multiple-value-bind (start-date type) (pass-info (pass-of student))
      (not (expired-p type start-date)))))

(defun validate-drop-in (student)
  "Validate drop-in according to today's date"
  (when (not (null (drop-in student)))
    (local-time:timestamp= (local-time:timestamp-minimize-part (time-now) :hour)
                           (local-time:timestamp-minimize-part (getf (first (drop-in student)) :date)
                                                               :hour))))

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
             (:div :id "header") ;page header
             (:div :id "container"
                   ,@body)))))

(define-easy-handler (main :uri "/main") ()
  (standard-page (:title "Ashtanga Yoga Osaka")
    (:table :class "maintable"
     (:caption "Students Today")
     (:col)
     (:tbody ;Displays list of students that attended today
      (:tr (:th "Name")
           (:th "Pass"))
      (dolist (student *students-today*)
        (htm
         (:tr
          (:td (:a :class "btn" :href (format nil "/remove-today?name=~A" (name student)) "X")
               (fmt "~A" (name student)))
          (cond ((equal (validate-drop-in student) t) (htm (:td (fmt "Drop-in")))) ; Check if Drop-in or not
                (t (htm (:td (fmt "~A" (get-type (pass-of student))))))))))))
    (:div :id "actionlist" 
          (:a :class "btn" :href "add-student-to-class" "Add Student")
          (:a :class "btn" :href "student-list" "Student List"))))

;; Remove from today's list
(define-easy-handler (remove-today :uri "/remove-today") (name) 
  (standard-page (:title "Ashtanga Yoga Osaka")
    (remove-from-today name)
    (redirect "/main")))

(define-easy-handler (student-list :uri "/student-list") ()
  (standard-page (:title "Ashtanga Yoga Osaka | Student List")
    (:table :class "maintable"
            (:caption "Student List")
            (:col)
            (:tbody
             (:tr (:th "Name")
                  (:th "Email")
                  (:th "Latest Pass"))
             (dolist (student *students*)
               (htm
                (:tr
                 (:td (fmt "~A" (name student)))
                 (:td (fmt "~A" (email student)))
                 (:td (fmt "~A" (cond ((pass-p student) ; retrieve the latest pass details if existent
                                       (print-month-day (get-start-date (pass-of student))))
                                      (t (pass student))))))))))
    (:div :id "actionlist"
          (:a :class "btn" :href "main" "Main"))))

(define-easy-handler (add-student-to-class :uri "/add-student-to-class") ()
  (standard-page (:title "Ashtanga Yoga Osaka | Add student to class")
    (:div :class "buttonLinkList"
          (dolist (student *students*)
            (htm
             (:a :class "btn" :href (format nil "/validate-and-add?name=~A" (name student))
                 (fmt "~A" (name student)))
             (:br))))
    (:div :id "actionlist"
          (:a :class "btn" :href "/main" "Main")
          (:a :class "btn" :href "/new-student-f" "New Student"))))

(define-easy-handler (new-student-f :uri "/new-student-f") ()
  (standard-page (:title "Ashtanga Yoga Osaka | Register New Student")
    (:h1 "Register New Student")
    (:form :action "/register-new-student" :method "post" :id "register-student"
           (:p "Name" (:input :type "text" :name "name" :class "txt"))
           (:p "Email" (:input :type "email" :name "email" :class "txt"))
           (:p (:input :type "submit" :value "Register" :class "btn")))))

(define-easy-handler (register-new-student :uri "/register-new-student") (name email)
  (register-student (new-student :name name :email email))
  (redirect (format nil "/buy-pass-drop-in-f?name=~A" name)))

;; Validate the pass and add the student to today's class
(define-easy-handler (validate-and-add :uri "/validate-and-add") (name)
  (let* ((student (student-from-name name))
         (validate-p (validate-pass student)))
    (cond ((equal validate-p t) (progn (push student *students-today*)
                                       (redirect "/main")))
          ((equal (validate-drop-in student) t) (progn (push student *students-today*)
                                                       (redirect "/main")))
          ((null (pass-p student)) (redirect (format nil "/buy-pass-drop-in-f?name=~A" name)))
          (t (redirect (format nil "/buy-pass-drop-in-f?name=~A" name))))))

(define-easy-handler (buy-pass-drop-in-f :uri "/buy-pass-drop-in-f") (name)
  (standard-page (:title "Ashtanga Yoga Osaka | Buy Pass or Drop-in")
    (:h1 "Buy pass or drop-in") 
    (:form :action (format nil "/add-pass-drop-in?name=~A" name) :method "post" :id "buy-pass-drop-in"
           (:p "Morning Pass" (:input :type "checkbox" :name "pass" :value 'm))
           (:p "Evening Pass" (:input :type "checkbox" :name "pass" :value 'e))
           (:p "1 Week Pass" (:input :type "checkbox" :name "pass" :value 'w))
           (:p "Drop-in" (:input :type "checkbox" :name "pass" :value 'd))
           (:p "Amount" (:input :type "number" :name "amt"))
           (:p (:input :type "submit" :value "Buy" :class "btn")))))

(define-easy-handler (add-pass-drop-in :uri "/add-pass-drop-in") (name pass amt)
  (cond ((equalp pass "M") (new-pass (student-from-name name)
                                     (make-pass :type 'm :start-date (time-now) :amt (parse-integer amt))))
        ((equalp pass "E") (new-pass (student-from-name name)
                                     (make-pass :type 'e :start-date (time-now) :amt (parse-integer amt))))
        ((equalp pass "W") (new-pass (student-from-name name)
                                     (make-pass :type 'w :start-date (time-now) :amt (parse-integer amt))))
        ((equalp pass "D") (new-drop-in (student-from-name name)
                                        (time-now)
                                        (parse-integer amt)))
        (t nil))
  (redirect "/main"))

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

(defun remove-last-pass (student)
  (setf (pass student) (rest (pass student))))

