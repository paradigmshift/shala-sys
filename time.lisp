(in-package #:shala-sys)

(defmacro lambda-map (list element &body body)
  `(mapcar #'(lambda (,element)
               ,@body)
           ,list))

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

(defun print-month-day (timestamp)
  (format nil "~A ~A" (print-month timestamp) (print-day timestamp)))

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
                                        :start-date (unix->local (elt pass 3))
                                        :amt (elt pass 5))))

(defun convert-passes (pass-list)
  (lambda-map pass-list pass (setf (getf pass :start-date)
                                   (local->unix (getf pass :start-date)))
              pass))

(defun convert-drop-in (drop-in-list)
  (lambda-map drop-in-list drop-in (setf (getf drop-in :date)
                                         (local->unix (getf drop-in :date)))))
;; (defun reconvert-passes (pass-list)
;;   (mapcar #'(lambda (pass)
;;               (make-pass :type (intern (elt pass 1) :shala-sys)
;;                          :start-date (unix->local (elt pass 3))
;;                          :amt (elt pass 5))) pass-list))

;; (defun convert-passes (pass-list)
;;   (mapcar #'(lambda (pass)
;;               (setf (getf pass :start-date)
;;                     (local->unix (getf pass :start-date)))
;;               pass)
;;           pass-list))

;; (defun convert-drop-in (drop-in-list)
;;   (mapcar #'(lambda (drop-in)
;;               (setf (getf drop-in :date)
;;                     (local->unix (getf drop-in :date)))
;;               drop-in)
;;           drop-in-list))

;; (defun reconvert-drop-in (drop-in-list)
;;   (mapcar #'(lambda (drop-in)
;;               (make-drop-in :date (unix->local (elt drop-in 1))
;;                             :amt (elt drop-in 3)))
;;           drop-in-list))
(defun adjust-days (timestamp days)
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

(defun get-pass-on (year month pass-list)
  "Returns the pass of the month"
  (first (remove-if-not #'(lambda (pass)
                                     (equalp (cons (local-time:timestamp-year (get-start-date pass))
                                                   (local-time:timestamp-month (get-start-date pass)))
                                             (cons year month)))
                                 pass-list)))

(defun prorate-till-month-end (pass)
  "Calculates the prorated amount of the pass based on purchase date and the number of days in the month. Returns as a second value the difference (carry-over for next month) between the total amount paid and the prorated amount."
  (let* ((month-days (local-time:days-in-month (local-time:timestamp-month (get-start-date pass))
                                              (local-time:timestamp-year (get-start-date pass))))
         (start-day (local-time:timestamp-day (get-start-date pass)))
         (prorated-amt (* (/ (float (getf pass :amt)) month-days)
                          (- month-days start-day))))
    (values prorated-amt
            (- (getf pass :amt)
               prorated-amt))))

(defun carry-over+current (pass)
  "Add the previous month's pass's carry-over to the current month's prorated amount"
  (let ((prev-month (get-pass-on (get-year (adjust-months (get-start-date pass) -1))
                                 (get-month (adjust-months (get-start-date pass) -1))))
        (current-month pass))
    (if (and prev-month current-month)
        (+ (prorate-till-month-end current-month)
           (nth-value 1 (prorate-till-month-end prev-month)))
        (nth-value 0 (prorate-till-month-end current-month)))))

(defun total-for (year month)
  "Retrieve the pass of the year and month given and return the carry-over + prorated amount if applicable, or just the carry-over amount from the previous pass (if existent) if there is no pass for the the given year and month."
  (let* ((timestamp (local-time:encode-timestamp 1 1 1 1 1 month year))
         (pass (if (get-pass-on year month)
                   (get-pass-on year month)
                   (get-pass-on (get-year (adjust-months timestamp -1))      ;No pass for the given year and month, try extracting the previous month's pass
                                (get-month (adjust-months timestamp -1))))))
    (when pass
      (if (equalp (get-month (get-start-date pass))                          ;Pass for given year and month exists
                  month)
          (carry-over+current pass)
          (nth-value 1 (prorate-till-month-end pass)))))) 