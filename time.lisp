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
