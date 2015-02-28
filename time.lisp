(in-package #:shala-sys)

;;;; Functions dealing with calls to the local-time API, and other time-related functions, are defined here.
(defmacro lambda-map (list element &body body)
  "Mapcar to lambda function"
  `(mapcar #'(lambda (,element)
               ,@body)
           ,list))

(defun expired-p (type startdate)
  "checks to see if the pass is expired based on type and startdate, compared with date today, time minimized to 0:00"
  (let ((duration (rest (assoc  type *type-map*))))
    (local-time:timestamp< (local-time:adjust-timestamp startdate (offset :day duration))
                           (local-time:timestamp-minimize-part (time-now) :hour))))

(defun validate-pass (student) 
  "Validate pass according to its type, validity based on *type-map*"
  (when (not (null (pass-p student)))
    (multiple-value-bind (start-date type) (pass-info (pass-of student))
      (if (or (equalp type 'm) (equalp type 'e))
          (and (not (expired-p type start-date))
               (equalp type (time-slots (local-time:timestamp-hour (time-now))))) 
          (not (expired-p type start-date))))))

(defun validate-drop-in (student)
  "Validate drop-in according to today's date"
  (when (not (null (drop-in student)))
    (local-time:timestamp= (local-time:timestamp-minimize-part (time-now) :hour)
                           (local-time:timestamp-minimize-part (getf (first (drop-in student)) :date)
                                                               :hour))))
(defun time-slots (hour)
  "Gives the time slot of the hour given. 'morning is morning mysore time slot, 'evening is the evening mysore time slot,
   and 'between is everything in between."
  (cond ((and (> hour 4) (< hour 10)) 'm)
        ((and (> hour 16) (< hour 21) 'e))
        (t 'between)))
