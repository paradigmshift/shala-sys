(in-package #:shala-sys)

;;;;  This is where all the general functions/macros are defined. Utilities are
;;;;classified as helper functions/macros that make up the highest layer of
;;;;abstraction.

;;------------------------------------------------------------------------------
;; Class instantiation
;;------------------------------------------------------------------------------

(defun new-expense (&key date comment amount)
  "Instantiate new expense"
  (make-instance 'expense :date date :comment comment :amount amount))

(defun new-student (&key name email)
  "Instantiate new student"
  (make-instance 'student :name name :email email))

(defun make-pass (&key type date amt)
  "Return a list with the pass information"
  (list :type type :date date :amt amt))

(defun make-drop-in (&key date amt)
  (list :date date :amt amt))

;;------------------------------------------------------------------------------
;; Filter and query utilities
;;------------------------------------------------------------------------------

(defun filter-by (&key (year nil year-p)
                    (month nil month-p)
                    (type nil type-p))
  "Remove members of the list not matching the given year and/or month and/or type, returns a
   function that takes the list"
  (lambda (lst)
    (remove-if-not #'(lambda (element)
                       (let* ((year-ok-p (or (not year-p)
                                             (equalp (local-time:timestamp-year (get-record-date element))
                                                     year)))
                              (month-ok-p (or (not month-p)
                                              (equalp (local-time:timestamp-month (get-record-date element))
                                                      month)))
                              (type-ok-p (or (not type-p) 
                                             (equalp (get-type element)
                                                     type)))
                              (all-ok-p (and year-ok-p month-ok-p type-ok-p)))
                         all-ok-p))
                        lst)))

(defun filter-by-year-month (year month)
  (filter-by :year year :month month))

(defun filter-by-year-month-type (year month type)
  (filter-by :year year :month month :type type))

(defun pass-of (student)
  "Retrieve latest pass from student"
  (first (pass student)))

(defun pass-p (student)
  "Test if student has pass, predicate"
  (not (null (pass student))))

(defun get-record-date (record)
  "Retrieve the date field from a plist"
  (getf record :date))

(defun get-type (pass)
  "Retrieve type from pass, m - morning, e - evening, w - 1 week"
  (getf pass :type))

(defun month-days-in-pass (pass)
  "Returns how many days in the month of the pass purchase date"
  (local-time:days-in-month (get-month (get-record-date pass))
                            (get-year (get-record-date pass))))

;;------------------------------------------------------------------------------
;; Timestamp utilities
;;------------------------------------------------------------------------------

(defun time-now ()
  (local-time:now))

(defun local->unix (timestamp)
  (local-time:timestamp-to-unix timestamp))

(defun unix->local (timestamp)
  (local-time:unix-to-timestamp timestamp))

(defun get-hour (timestamp)
  "Extract the hour from the timestamp"
  (local-time:timestamp-hour timestamp))

(defun get-year (timestamp)
  "Extract the year from the timestamp"
  (local-time:timestamp-year timestamp))

(defun get-month (timestamp)
  "Extract the month from the timestamp"
  (local-time:timestamp-month timestamp))

(defun get-day (timestamp)
  "Extract the day from the timestamp"
  (local-time:timestamp-day timestamp))

(defun print-year (timestamp)
  (local-time:format-timestring nil timestamp :format '(:year)))

(defun print-month (timestamp)
  (local-time:format-timestring nil timestamp :format '(:short-month)))

(defun print-day (timestamp)
  (local-time:format-timestring nil timestamp :format '(:day)))

(defun print-year-month-day (timestamp)
  (local-time:format-timestring nil timestamp :format '(:year #\- :month #\- :day)))

(defun print-year-month-day->timestamp (timestamp)
  "Convert string yy-mm-dd to local-time timestamp"
  (when (not (typep timestamp 'local-time:timestamp))
    (let ((date (mapcar #'(lambda (s)
                            (parse-integer s))
                        (split-sequence:split-sequence #\- timestamp))))
      (local-time:timestamp-minimize-part (local-time:encode-timestamp 0 0 0 0 (elt date 2)
                                                                       (elt date 1)
                                                                       (elt date 0))
                                          :hour))))

(defun print-month-day (timestamp)
  (when timestamp
    (format nil "~A ~A" (print-month timestamp) (print-day timestamp))))

(defun adjust-days (timestamp days)
  "Add or subract days from the timestamp, returns a new timestamp"
  (local-time:adjust-timestamp timestamp (offset :day days)))

(defun adjust-months (timestamp months)
  "Add or subtract months from the timestamp, returns a new timestamp"
  (local-time:adjust-timestamp timestamp (offset :month months)))
