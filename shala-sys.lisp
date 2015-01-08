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

(defmethod print-object ((object student) stream)
  "Make the student class information readable from the REPL"
  (print-unreadable-object (object stream :type t)
    (with-slots (name email pass drop-in) object
      (format stream "name: ~s email: ~s pass: ~s drop-in :~s" name email pass drop-in))))

(defun new-student (&key name email)
  "Instantiate new student"
  (make-instance 'student :name name :email email))

(defun make-pass (&key type date amt)
  "Return a list with the pass information"
  (list :type type :date date :amt amt))

(defun make-drop-in (&key date amt)
  (list :date date :amt amt))

(defun pass-of (student)
  "Retrieve latest pass from student"
  (first (pass student)))

(defun pass-p (student)
  "Test if student has pass, predicate"
  (not (null (pass student))))

(defun get-record-date (record)
  "Retrieve the start-date from the pass"
  (getf record :date))

(defun get-type (pass)
  "Retrieve type from pass, m - morning, e - evening, w - 1 week"
  (getf pass :type))

(defun remove-nil (list)
  "Remove nil elements from list"
  (remove-if #'null list))

(defun sum (list)
  (reduce #'+ list))

(defun pass-info (pass)
  "Retrieve start-date and type of pass"
  (values (get-record-date pass) (get-type pass)))

(defun remove-from-today (name)
  "Remove student from today's list"
  (setf *students-today* (remove-if #'(lambda (student)
                                        (equalp (name student) name))
                                    *students-today*)))

(defun prorate-till-month-end (pass)
  "Calculates the prorated amount of the pass based on purchase date and the number
   of days in the month. Returns as a second value the difference (carry-over for next month)
   between the total amount paid and the prorated amount."
  (let* ((month-days ;; (local-time:days-in-month (local-time:timestamp-month (get-record-date pass))
                     ;;                          (local-time:timestamp-year (get-record-date pass)))
          (month-days-in-pass pass))
         (start-day ;; (local-time:timestamp-day (get-record-date pass))
           (get-day (get-record-date pass)))
         ;;Monthly passes are prorated by dividing by days of the month, weekly passes by 7 days
         (divisor (if (equalp (getf pass :type) 'w)
                      7
                      month-days))
         (prorated-amt (* (/ (float (getf pass :amt)) divisor)
                          (1+                                               ;Passes start the day
                                                                            ; they are bought
                           (- month-days start-day)))))
    (values prorated-amt
            (- (getf pass :amt)
               prorated-amt))))

(defun carry-over+current (pass &optional prev-pass)
  "Add the previous month's pass's carry-over to the current month's prorated amount"
  (let ((prev-month prev-pass)
        (current-month pass))
    (if (and prev-month current-month)
        (+ (prorate-till-month-end current-month)
           (nth-value 1 (prorate-till-month-end prev-month)))
        (nth-value 0 (prorate-till-month-end current-month)))))

(defun pass-total-for (year month student)
  "Retrieve the pass of the year and month given and return the carry-over + prorated
   amount if applicable, or just the carry-over amount from the previous pass (if existent)
   if there is no pass for the the given year and month."
  (let* ((timestamp (local-time:encode-timestamp 1 1 1 1 1 month year))
         (pass (if (get-pass-on year month (pass student))                   ;Search for pass from
                                                                             ; pass list
                   (get-pass-on year month (pass student))
                   ;;No pass for the given year and month, try extracting the previous month's pass
                   (get-pass-on (get-year (adjust-months timestamp -1))
                                (get-month (adjust-months timestamp -1))
                                (pass student)))))
    (when pass
      (if (equalp (get-month (get-record-date pass))                          ;Pass for given year and
                                                                             ; month exists
                  month)
          (carry-over+current pass
                              (get-pass-on (get-year (adjust-months (get-record-date pass) -1))
                                           (get-month (adjust-months (get-record-date pass) -1))
                                           (pass student)))
          ;; Pass for given month doesn't exist, defaulting to carry-over from last month
          (nth-value 1 (prorate-till-month-end pass))))))  

(defun drop-in-total-for (year month student)
  (sum (mapcar #'(lambda (drop-in)
                   (getf drop-in :amt))
               (get-drop-in-on year month (drop-in student)))))

(defun student-total-for (year month student)
  (+ (pass-total-for year month student)
     (drop-in-total-for year month student)))

(defun monthly-total (year month)
  "Total of all monthly revenue"
  (sum (remove-nil (mapcar #'(lambda (student)
                                      (pass-total-for year month student))
                           (students)))))
