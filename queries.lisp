(in-package #:shala-sys)

(defun get-pass-on (year month pass-list)
  "Returns the pass of the month"
  (first (remove-if-not #'(lambda (pass)
                            (equalp (cons (local-time:timestamp-year (get-record-date pass))
                                          (local-time:timestamp-month (get-record-date pass)))
                                    (cons year month)))
                        pass-list)))

(defun get-drop-in-on (year month drop-in-list)
  (remove-if-not #'(lambda (drop-in)
                     (equalp (cons (local-time:timestamp-year (get-record-date drop-in))
                                   (local-time:timestamp-month (get-record-date drop-in)))
                             (cons year month)))
                 drop-in-list))
