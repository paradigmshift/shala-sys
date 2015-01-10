(in-package #:shala-sys)

(defmacro lambda-map (list element &body body)
  "Mapcar to lambda function"
  `(mapcar #'(lambda (,element)
               ,@body)
           ,list))

(defun time-now ()
  (local-time:now))

(defun local->unix (timestamp)
  (local-time:timestamp-to-unix timestamp))

(defun unix->local (timestamp)
  (local-time:unix-to-timestamp timestamp))

(defun expired-p (type startdate)
  "checks to see if the pass is expired based on type and startdate, compared with date today, time minimized to 0:00"
  (let ((duration (rest (assoc  type *type-map*))))
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
(defun reconvert-drop-in (drop-in-list)
  (lambda-map drop-in-list drop-in (make-drop-in :date (unix->local (elt drop-in 1))
                                                 :amt (elt drop-in 3))))

(defun reconvert-passes (pass-list)
  (lambda-map pass-list pass (make-pass :type (intern (elt pass 1) :shala-sys)
                                        :date (unix->local (elt pass 3))
                                        :amt (elt pass 5))))

(defun convert-passes (pass-list)
  (lambda-map pass-list pass (setf (getf pass :date)
                                   (local->unix (getf pass :date)))
              pass))

(defun convert-drop-in (drop-in-list)
  (lambda-map drop-in-list drop-in (setf (getf drop-in :date)
                                         (local->unix (getf drop-in :date)))
              drop-in))

(defun adjust-days (timestamp days)
  "Add or subract days from the timestamp, returns a new timestamp"
  (local-time:adjust-timestamp timestamp (offset :day days)))

(defun adjust-months (timestamp months)
  "Add or subtract months from the timestamp, returns a new timestamp"
  (local-time:adjust-timestamp timestamp (offset :month months)))

(defun get-year (timestamp)
  "Extract the year from the timestamp"
  (local-time:timestamp-year timestamp))

(defun get-month (timestamp)
  "Extract the month from the timestamp"
  (local-time:timestamp-month timestamp))

(defun get-day (timestamp)
  "Extract the day from the timestamp"
  (local-time:timestamp-day timestamp))

(defun month-days-in-pass (pass)
  (local-time:days-in-month (get-month (get-record-date pass))
                            (get-year (get-record-date pass))))

;; (defun print-month (timestamp)
;;   (local-time:format-timestring nil timestamp :format '(:short-month)))

;; (defun print-day (timestamp)
;;   (local-time:format-timestring nil timestamp :format '(:day)))

;; (defun print-month-day (timestamp)
;;   (format nil "~A ~A" (print-month timestamp) (print-day timestamp)))

;; (defun filter-by (&key year month type)
;;   "Remove members of the list not matching the given year and/or month and/or type, returns a
;;    function that takes the list"
;;   (lambda (lst)
;;     (remove-if-not #'(lambda (element)
;;                             (when year
;;                               (format t "~A" (equalp (local-time:timestamp-year (get-record-date element))
;;                                                      year)))
;;                             (when month
;;                               (format t "~A" (equalp (local-time:timestamp-month (get-record-date element))
;;                                                      month)))
;;                             (when type
;;                               (format t "~A" (equalp (get-type element)
;;                                                      type))))
;;                         lst)))

;; (defun filter-by-year-month (year month)
;;   (filter-by :year year :month month))

;; (defun filter-by-year-month-type (year month type)
;;   (filter-by :year year :month month :type type))

;; (defun get-drop-in-on (year month drop-in-list)
;;   "Retrieve drop-in for given year and month from a list of drop-ins"
;;   (funcall (filter-by-year-month year month) drop-in-list))

;; (defun get-pass-on (year month pass-list)
;;   "Retrieve pass for given year and month from a list of passes"
;;   (first (funcall (filter-by-year-month year month) pass-list)))

;; (defun get-morning-passes-on (year month pass-list)
;;   "Retrieve all 'm (morning) type passes for given year and month from a list of passes"
;;   (funcall (filter-by-year-month-type year month 'm) pass-list))
