(in-package #:shala-sys)

(defun get-all-passes ()
  (apply #'append (mapcar #'(lambda (student)
                              (pass student)) (students))))

;; (defun get-pass-on (year month pass-list)
;;   "Returns the pass of the month"
;;   (first (remove-if-not #'(lambda (pass)
;;                             (equalp (cons (local-time:timestamp-year (get-record-date pass))
;;                                           (local-time:timestamp-month (get-record-date pass)))
;;                                     (cons year month)))
;;                         pass-list)))

;; (defun get-drop-in-on (year month drop-in-list)
;;   (remove-if-not #'(lambda (drop-in)
;;                      (equalp (cons (local-time:timestamp-year (get-record-date drop-in))
;;                                    (local-time:timestamp-month (get-record-date drop-in)))
;;                              (cons year month)))
;;                  drop-in-list))

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
