;;------------------------------------------------------------------------------
;; Expense utilities
;;------------------------------------------------------------------------------

(defun register-expense (expense)
  "Insert EXPENSE into the DB"
  (db.insert *expense-list* (expense->doc expense)))

(defun expenses ()
  "Retrieves the list of expenses from the DB, sorted"
  (map 'list #'(lambda (doc)
                 (doc->expense doc))
       (docs (iter (db.sort *expense-list* :all :field "date")))))

;;; Expense to DB and back again
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

;;; Expense date manipulation
(defun expense-date-> (expense &key fn)
  "Higher-order function for manipulating EXPENSE's date."
  (let ((expense-copy (copy-tree expense)))
    (setf (date expense-copy)
          (funcall fn (date expense-copy)))
    expense-copy))

(defun expense-date->yy-mm-dd (expense)
  "Convert EXPENSE's date to yy-mm-dd format"
  (expense-date-> expense :fn #'print-year-month-day))

(defun expense-list-dates->yy-mm-dd (expense-list)
  "Convert EXPENSE-LIST's dates to yy-mm-dd format."
  (mapcar #'(lambda (expense)
              (expense-date->yy-mm-dd expense))
          expense-list))

(defun expense-and-expense-list-dates->yy-mm-dd (expense expense-list)
  "Convert EXPENSE and EXPENSE-LIST's dates to yy-mm-dd format."
  (values (expense-date->yy-mm-dd expense)
          (expense-list-dates->yy-mm-dd expense-list)))

;;------------------------------------------------------------------------------
;; Pass utilities
;;------------------------------------------------------------------------------

;;; Pass date manipulation
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

;;------------------------------------------------------------------------------
;; Student utilities
;;------------------------------------------------------------------------------

(defun students ()
  "Retrieves the list of students from the DB, sorted"
  (map 'list #'(lambda (doc)
                 (doc->student doc))
       (docs (iter (db.sort *student-list* :all :field "name")))))

(defun student-from-name (name)
  "Find student based on NAME"
  (let ((found-students (docs (db.find *student-list* ($ "name" name)))))
    (when found-students
      (doc->student (first found-students)))))

(defun register-student (student)
  "Insert STUDENT into DB."
  (db.insert *student-list* (student->doc student)))

;;; Student to DB and back
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
