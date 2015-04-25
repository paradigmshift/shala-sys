(in-package #:shala-sys)

;;;;  This is where all the utility functions/macros are defined. Utilities are classified as helper
;;;;functions/macros that make up the highest layer of abstraction.

;;;; Filter/query functionality

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

(defun get-record-date (record)
  "Retrieve the start-date from the pass"
  (getf record :date))

(defun get-type (pass)
  "Retrieve type from pass, m - morning, e - evening, w - 1 week"
  (getf pass :type))

(defun month-days-in-pass (pass)
  "Returns how many days in the month of the pass purchase date"
  (local-time:days-in-month (get-month (get-record-date pass))
                            (get-year (get-record-date pass))))

;;;; Pass functionality

;; (defun pass-list-dates->yy-mm-dd (pass-list)
;;   "Converts the date fields of pass-list to a string in yy-mm-dd format"
;;   (map 'list #'(lambda (pass)
;;                  (setf (getf pass :date)
;;                        (print-year-month-day (get-record-date pass))))
;;        pass-list)
;;   pass-list)

(defun pass-list-dates->yy-mm-dd (pass-list)
  "Converts the date fields of pass-list to a string in yy-mm-dd format"
  (pass-list-dates-> pass-list :fn #'print-year-month-day))

(defun pass-list-dates->timestamp (pass-list)
  "Converts the date fields of pass-list from yy-mm-dd to a local-time:timestamp"
  (map 'list #'(lambda (pass)
                 (when (not (typep (getf pass :date) 'local-time:timestamp))
                   (setf (getf pass :date)
                         (print-year-month-day->timestamp (get-record-date pass)))))
       pass-list)
  pass-list)

(defun sort-pass-dates (pass-list)
  "Sorts the list of passes according to dates in descending order. It first transforms the dates from
   yy-mm-dd to local-time, then from local-time to unix, sorts, then back again to local-time"
  (let ((pass-list-copy (copy-tree pass-list)))
    (let ((pass-list-copy
           (pass-list-dates-> (pass-list-dates->timestamp pass-list-copy)
                              :fn #'local->unix)))
      (sort pass-list-copy #'> :key #'get-record-date)
      (pass-list-dates-> pass-list-copy :fn #'unix->local))))

(defun pass-list-dates-> (pass-list &key fn)
  "Converts the date fields of a list of passes based on the function passed to fn. For example, if
   if unix->local is passed in, it will try to convert the date field from unix time to local-time."
  (let ((pass-list-copy (copy-tree pass-list)))
    (map 'list #'(lambda (pass)
                   (setf (getf pass :date)
                         (funcall fn (get-record-date pass))))
         pass-list-copy)
    pass-list-copy))

(defun pass-dates-unix->local (pass-list)
  (let ((pass-list-copy (copy-tree pass-list)))
    (pass-list-dates-> pass-list-copy :fn #'unix->local)))

(defun pass-dates-local->unix (pass-list)
  (let ((pass-list-copy (copy-tree pass-list)))
    (pass-list-dates-> pass-list-copy :fn #'local->unix)))

;;; Student plist and MongoDB functionality

(defun drop-in-doc->plist (drop-in-list)
  (mapcar #'(lambda (drop-in)
              (make-drop-in :date (elt drop-in 1)
                            :amt (elt drop-in 3)))
          drop-in-list))

(defun pass-doc->plist (pass-list)
  (mapcar #'(lambda (pass)
              (make-pass :type (intern (elt pass 1) :shala-sys)
                         :date (elt pass 3)
                         :amt (elt pass 5)))
          pass-list))

(defun student->doc (student)
  "Creates MongoDB document from student object"
  (with-slots (name email pass drop-in attendance) student
    ($ ($ "name" name)
       ($ "email" email)
       ($ "pass" (pass-dates-local->unix pass))
       ($ "drop-in" (pass-dates-local->unix drop-in))
       ($ "attendance" attendance))))

(defun doc->student (doc)
  "Creates student object from MongoDB document"
  (make-instance 'student :name (get-element "name" doc)
                 :email (get-element "email" doc)
                 :pass (pass-dates-unix->local (pass-doc->plist (get-element "pass" doc))) ;pass plist is converted to string when saved in the database
                 :drop-in (pass-dates-unix->local (drop-in-doc->plist (get-element "drop-in" doc)))
                 :attendance (get-element "attendance" doc)))

(defun expense->doc (expense)
  "Creates MongoDB document from expense object"
  (with-slots (date comment amount) expense
    ($ ($ "date" (local->unix date))
       ($ "comment" comment)
       ($ "amount" amount))))

(defun doc->expense (doc)
  "Creates expense object from MongoDB document"
  (make-instance 'expense :date (unix->local (get-element "date" doc))
                 :comment (get-element "comment" doc)
                 :amount (get-element "amount" doc)))

(defun register-expense (expense)
  (db.insert *expense-list* (expense->doc expense)))

(defun expenses ()
  "Retrieves the list of expenses from the DB, sorted"
  (map 'list #'(lambda (doc)
                 (doc->expense doc))
       (docs (iter (db.sort *expense-list* :all :field "date")))))

(defun students ()
  "Retrieves the list of students from the DB, sorted"
  (map 'list #'(lambda (doc)
                 (doc->student doc))
       (docs (iter (db.sort *student-list* :all :field "name")))))

(defun student-from-name (name)
  (let ((found-students (docs (db.find *student-list* ($ "name" name)))))
    (when found-students
      (doc->student (first found-students)))))

(defun register-student (student)
  (db.insert *student-list* (student->doc student)))

;;;; Timestamp functionality

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
