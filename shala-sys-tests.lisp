(in-package :shala-sys)


(defun reset-students ()
  (setf *students* '())
  (setf *students-today* '()))

(defun populate-dummy-students ()
  (progn
    (register-student (new-student :name "yukako kotani" :email "test@gmail.com"))
    (register-student (new-student :name "kaori kawai" :email "test2@gmail.com"))
    (register-student (new-student :name "yoko kamioka" :email "test3@gmail.com"))
    (register-student (new-student :name "taeko fujimoto" :email "test4@gmail.com"))))

(defun populate-dummy-passes ()
  (progn 
    (new-pass (student-from-name "yukako kotani") (make-pass :type 'm
                                                             :start-date (local->unix (add-months -2 (time-now)))
                                                             :amt 17000)) ;; Has one pass that was bought 2 months ago (1 month type).
    (new-pass (student-from-name "kaori kawai") (make-pass :type 'w
                                                           :start-date (local->unix (add-days -6 (time-now)))
                                                           :amt 6000)) ;; Has one pass that was bought 6 days ago (1 week type).
    (new-pass (student-from-name "yoko kamioka") (make-pass :type 'm
                                                            :start-date (local->unix (time-now))
                                                            :amt 17000)) ;; Has one pass that was bought now (1 month type)
    (new-pass (student-from-name "taeko fujimoto") (make-pass :type 'm
                                                              :start-date (local->unix (add-days -45 (time-now)))
                                                              :amt 17000)))) ;; Has one pass that was bought 45 days ago (1 month type)

(defun last-month ()
  (local-time:adjust-timestamp (time-now) (offset :month -1)))

(defun new-pass-today (type amt)
  (new-pass :type type :amt amt :start-date (time-now)))
