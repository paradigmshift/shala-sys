(in-package #:shala-sys)

;;;; Queries/filtering functions are defined here. 

(defun this-month-passes ()
  "Returns this month's passes as 3 values, morning passes, evening passes, week passes"
  (passes-on (get-year (time-now)) (get-month (time-now))))

(defun passes-on (year month)
  "Retrieve passes on given year and month as 3 values, morning passes, evening passes, and week passes"
  (multiple-value-bind (morning evening week) (categorize-passes year month (students-with-pass-on year month))
      (values morning evening week)))

(defun pass-total-for (year month student)
  "Retrieve the pass of the year and month given and return the carry-over + prorated
   amount if applicable, or just the carry-over amount from the previous pass (if existent)
   if there is no pass for the the given year and month."
  (let ((pass (current-or-prev-pass-on year month student)))
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

(defun current-or-prev-pass-on (year month student)
  "Returns the current month's or last month's pass"
  (let* ((timestamp (local-time:encode-timestamp 1 1 1 1 1 month year))
         (pass (if (get-pass-on year month (pass student))                   ;Search for pass from
                                                                             ; pass list
                   (get-pass-on year month (pass student))
                   ;;;No pass for the given year and month, try extracting the previous month's pass
                   (get-pass-on (get-year (adjust-months timestamp -1))
                                (get-month (adjust-months timestamp -1))
                                (pass student)))))
    pass))

(defun weekly-passes-on (year month student)
  "Retrieves the weekly passes on the given year and month, checks also for carry-over
   from the previous month"
  (let* ((timestamp (local-time:encode-timestamp 1 1 1 1 1 month year))
         (prev-month-passes (remove-if #'(lambda (pass)
                                           (= 0
                                              (nth-value 1 (prorate-till-month-end pass))))
                                       (get-week-passes-on (get-year (adjust-months timestamp -1))
                                                           (get-month (adjust-days timestamp -1))
                                                           (pass student)))))
    (append (get-week-passes-on year month (pass student))
            prev-month-passes)))

(defun week-pass-total-for (year month student)
  "Adds the weekly pass total for the given year and month"
  (let ((passes (weekly-passes-on year month student)))
    (reduce #'+ (map 'list #'(lambda (pass)
                               ;; weekly pass that was bought the month before but has carry-over
                               (if (not (= (get-month (get-record-date pass))
                                           month))
                                   (nth-value 1 (prorate-till-month-end pass))
                                   (prorate-till-month-end pass)))
                     passes))))

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

(defun students-with-pass-on (year month)
  "Returns a list of students with passes on given year and month"
  (remove-if-not #'(lambda (student)
                     (current-or-prev-pass-on year month student))
                 (students)))

(defun prorate-till-month-end (pass)
  "Calculates the prorated amount of the pass based on purchase date and the number
   of days in the month. Returns as a second value the difference (carry-over for next month)
   between the total amount paid and the prorated amount."
  (when pass
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
                            (1+             ;Passes start the day they are bought
                             (if (and (= divisor 7) ; means that the pass is a week pass
                                      (> (- month-days start-day) 7)) ; there is no carry-over because
                                 6                                    ;the week pass finished within the
                                        ;month
                                 (- month-days start-day))))))
      (values prorated-amt
              (- (getf pass :amt)
                 prorated-amt)))))

(defun carry-over+current (pass &optional prev-pass)
  "Add the previous month's pass's carry-over to the current month's prorated amount"
  (let ((prev-month prev-pass)
        (current-month pass))
    (if (and prev-month current-month)
        (+ (prorate-till-month-end current-month)
           (nth-value 1 (prorate-till-month-end prev-month)))
        (nth-value 0 (prorate-till-month-end current-month)))))

;; (defun categorize-passes (year month student-list)
;;   "Categorizes students based on their pass-type on given year and month"
;;   (let ((morning '())
;;         (evening '())
;;         (week '()))
;;     (map 'list #'(lambda (student)
;;                    (let ((pass-type (get-type (current-or-prev-pass-on year month student))))
;;                      (cond ((equalp pass-type 'm)
;;                             (push student morning))
;;                            ((equalp pass-type 'e)
;;                             (push student evening))
;;                            (t (push student week)))))
;;          student-list)
;;     (values morning evening week)))

(defun categorize-passes (year month student-list)
  "Categorizes students based on their pass-type on given year and month."
  (let ((morning '())
        (evening '())
        (week '()))
    (map 'list #'(lambda (student)
                   (let ((timestamp (local-time:encode-timestamp 1 1 1 1 1 month year)))
                     (when (or (not (null (get-morning-passes-on year month (pass student))))
                               (not (null (nth-value 1 (prorate-till-month-end (first (get-morning-passes-on (get-year (adjust-months timestamp -1))
                                                                                                             (get-month (adjust-months timestamp -1))
                                                                                                             (pass student))))))))
                       (push student morning))
                     (when (or (not (null (get-evening-passes-on year month (pass student))))
                               (not (null (nth-value 1 (prorate-till-month-end (first (get-evening-passes-on (get-year (adjust-months timestamp -1))
                                                                                                             (get-month (adjust-months timestamp -1))
                                                                                                             (pass student))))))))
                       (push student evening))
                     (when (or (not (null (get-week-passes-on year month (pass student))))
                               (not (null (nth-value 1 (prorate-till-month-end (first (get-week-passes-on (get-year (adjust-months timestamp -1))
                                                                                                             (get-month (adjust-months timestamp -1))
                                                                                                             (pass student))))))))
                       (push student week))))
         student-list)
    (values morning evening week)))

(defun pass-info (pass)
  "Retrieve start-date and type of pass"
  (values (get-record-date pass) (get-type pass)))

(defun get-drop-in-on (year month drop-in-list)
  "Retrieve drop-in for given year and month from a list of drop-ins"
  (funcall (filter-by-year-month year month) drop-in-list))

(defun get-pass-on (year month pass-list)
  "Retrieve pass for given year and month from a list of passes"
  (first (funcall (filter-by-year-month year month) pass-list)))

(defun get-morning-passes-on (year month pass-list)
  "Retrieve all 'm (morning) type passes for given year and month from a list of passes"
  (funcall (filter-by-year-month-type year month 'm) pass-list))

(defun get-evening-passes-on (year month pass-list)
  "Retrieve all 'e (evening) type passes for given year and month from a list of passes"
  (funcall (filter-by-year-month-type year month 'e) pass-list))

(defun get-week-passes-on (year month pass-list)
  "Retrieve all 'w (week) type passes for given year and month from a list of passes"
  (funcall (filter-by-year-month-type year month 'w) pass-list))

(defun get-morning-pass-on (year month pass-list)
  "Retrieve the morning pass from the given year and month from the list of passes
   (this is meant to be an individual student's list of passes)"
  (first (funcall (filter-by-year-month-type year month 'm) pass-list)))

(defun get-evening-pass-on (year month pass-list)
  "Retrieve the evening pass from the given year and month from the list of passes
   (this is meant to be an individual student's list of passes)"
  (first (funcall (filter-by-year-month-type year month 'e) pass-list)))
