;;;; shala-sys.lisp

(in-package #:shala-sys)

;;; "shala-sys" goes here. Hacks and glory await!
(defvar *students* '())

(defparameter *type-map* '((m . 30)
                           (e . 30)
                           (w . 6)))

;; (defun new-student (&key name email pass)
;;   "Return a list with the student's information, pass argument should be a list with type of pass, start date, and amount paid"
;;   (list :name name :email email :pass pass))

(defun new-student (&key name email)
  (make-instance 'student :name name :email email))

(defclass student ()
  ((name :initarg :name
         :accessor name)
   (email :initarg :email
          :accessor email)
   (pass :accessor pass)
   (drop-in :accessor drop-in)
   (attendance :accessor attendance)))

(defun register-student (student)
  (push student *students*))

(defun new-pass (&key type start-date amt)
  "Return a list with the pass information"
  (list :type type :start-date start-date :amt amt))

(defun update-student-pass (student pass)
  (push pass (pass studnet)))

;; (defun update-student-pass (student pass)
;;   "Push a new pass into the student's records"
;;   (push pass (getf student :pass)))

(defun pass-of (student)
  "Retrieve pass from student"
  (first (getf student :pass)))

(defun get-start-date (pass)
  "Retrieve the start-date from the pass"
  (getf pass :start-date))

(defun get-type (pass)
  "Retrieve type from pass, m - morning, e - evening, w - 1 week"
  (getf pass :type))

(defun validate-pass (student &key unit) ()
  "Validate pass according to unit (m for month, w for week)"
  (local-time:timestamp> 
   (local-time:timestamp+ (getf (pass-of student)
                                :start-date)
                         30 :day)
   (local-time:now)))

(defun pass-p (student)
  "Test if student has pass, predicate"
  (not (null (getf student :pass))))

(defun expired-p (type startdate)
  "checks to see if the pass is expired based on type and startdate, compared with date today, time minimized to 0:00"
  (let ((duration (rest (assoc type *type-map*))))
    (local-time:timestamp< (local-time:adjust-timestamp startdate (offset :day duration))
                           (local-time:timestamp-minimize-part (time-now) :hour))))

(defun pass-info (pass)
  (values (get-start-date pass) (get-type pass)))


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

;;;;; dummy data
;; (register-student (new-student :name "yukako" :email "test@gmail.com"))
;; (register-student (new-student :name "kaori" :email "test2@gmail.com"))
;; (register-student (new-student :name "taeko" :email "test3@gmail.com" :pass (list (new-pass :type 'm :start-date (last-month) :amt 17000))))

;; (defun last-month ()
;;   (local-time:adjust-timestamp (time-now) (offset :month -1)))

;; (defun new-pass-today (type amt)
;;   (new-pass :type type :amt amt :start-date (time-now)))
