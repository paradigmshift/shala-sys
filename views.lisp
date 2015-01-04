(in-package :shala-sys)

(defun start-server (port)
  (start (make-instance 'hunchentoot:easy-acceptor :port port)))

(defun serve-static-file (fname uri)
  "Push the filename's URI to the dispatch table"
  (push (create-static-file-dispatcher-and-handler fname uri)
        *dispatch-table*))

(defun load-dev ()
  (start-server 8080)
  (serve-static-file "/style.css" "/home/mo/dev/lisp/shala-sys/style.css")
  (serve-static-file "/21.jpg" "/home/mo/dev/lisp/shala-sys/21.jpg"))

;;;;HTML Views
(defmacro standard-page ((&key title script) &body body)
  `(with-html-output-to-string
       (*standard-output* nil :prologue t :indent t)
     (:html :lang "en"
            (:head
             (:meta :charset "utf-8")
             (:meta :name "viewport" :content "width=device-width, initial-scale=1") ;mobile friendly

             (:title ,title)
             (:link :type "text/css"
                    :rel "stylesheet"
                    :href "/style.css")
             ,(when script
                `(:script :type "text/javascript"
                          (str, script))))
            (:body
             (:div :id "header") ;page header
             (:div :id "container"
                   ,@body)))))

(define-easy-handler (main :uri "/main") ()
  (standard-page (:title "Ashtanga Yoga Osaka")
    (:table :class "maintable"
     (:caption "Students Today")
     (:col)
     (:tbody ;Displays list of students that attended today
      (:tr (:th "Name")
           (:th "Pass"))
      (dolist (student *students-today*)
        (htm
         (:tr
          (:td (:a :class "btn" :href (format nil "/remove-today?name=~A" (name student)) "X")
               (fmt "~A" (name student)))
          (cond ((equal (validate-drop-in student) t) (htm (:td (fmt "Drop-in")))) ; Check if Drop-in or not
                (t (htm (:td (fmt "~A" (get-type (pass-of student))))))))))))
    (:div :id "actionlist" 
          (:a :class "btn" :href "add-student-to-class" "Add Student")
          (:a :class "btn" :href "student-list" "Student List"))))

;; Remove from today's list
(define-easy-handler (remove-today :uri "/remove-today") (name) 
  (standard-page (:title "Ashtanga Yoga Osaka")
    (remove-from-today name)
    (redirect "/main")))

(define-easy-handler (student-list :uri "/student-list") ()
  (standard-page (:title "Ashtanga Yoga Osaka | Student List")
    (:table :class "maintable"
            (:caption "Student List")
            (:col)
            (:tbody
             (:tr (:th "Name")
                  (:th "Email")
                  (:th "Latest Pass"))
             (dolist (student (students))
               (htm
                (:tr
                 (:td (fmt "~A" (name student)))
                 (:td (fmt "~A" (email student)))
                 (:td (fmt "~A" (cond ((pass-p student) ; retrieve the latest pass details if existent
                                       (print-month-day (get-start-date (pass-of student))))
                                      (t (pass student))))))))))
    (:div :id "actionlist"
          (:a :class "btn" :href "main" "Main"))))

(define-easy-handler (add-student-to-class :uri "/add-student-to-class") ()
  (standard-page (:title "Ashtanga Yoga Osaka | Add student to class")
    (:div :class "buttonLinkList"
          (dolist (student (students))
            (htm
             (:a :class "btn" :href (format nil "/validate-and-add?name=~A" (name student))
                 (fmt "~A" (name student)))
             (:br))))
    (:div :id "actionlist"
          (:a :class "btn" :href "/main" "Main")
          (:a :class "btn" :href "/new-student-f" "New Student"))))

(define-easy-handler (new-student-f :uri "/new-student-f") ()
  (standard-page (:title "Ashtanga Yoga Osaka | Register New Student")
    (:h1 "Register New Student")
    (:form :action "/register-new-student" :method "post" :id "register-student"
           (:p "Name" (:input :type "text" :name "name" :class "txt"))
           (:p "Email" (:input :type "email" :name "email" :class "txt"))
           (:p (:input :type "submit" :value "Register" :class "btn")))))

(define-easy-handler (register-new-student :uri "/register-new-student") (name email)
  (register-student (new-student :name name :email email))
  (redirect (format nil "/buy-pass-drop-in-f?name=~A" name)))

;; Validate the pass and add the student to today's class
(define-easy-handler (validate-and-add :uri "/validate-and-add") (name)
  (let* ((student (student-from-name name))
         (validate-p (validate-pass student)))
    (cond ((equal validate-p t) (progn (push student *students-today*)
                                       (redirect "/main")))
          ((equal (validate-drop-in student) t) (progn (push student *students-today*)
                                                       (redirect "/main")))
          ((null (pass-p student)) (redirect (format nil "/buy-pass-drop-in-f?name=~A" name)))
          (t (redirect (format nil "/buy-pass-drop-in-f?name=~A" name))))))

(define-easy-handler (buy-pass-drop-in-f :uri "/buy-pass-drop-in-f") (name)
  (standard-page (:title "Ashtanga Yoga Osaka | Buy Pass or Drop-in")
    (:h1 "Buy pass or drop-in") 
    (:form :action (format nil "/add-pass-drop-in?name=~A" name) :method "post" :id "buy-pass-drop-in"
           (:p "Morning Pass" (:input :type "checkbox" :name "pass" :value 'M))
           (:p "Evening Pass" (:input :type "checkbox" :name "pass" :value 'E))
           (:p "1 Week Pass" (:input :type "checkbox" :name "pass" :value 'W))
           (:p "Drop-in" (:input :type "checkbox" :name "pass" :value 'D))
           (:p "Amount" (:input :type "number" :name "amt"))
           (:p (:input :type "submit" :value "Buy" :class "btn")))))

(define-easy-handler (add-pass-drop-in :uri "/add-pass-drop-in") (name pass amt)
  (cond ((equalp pass "M") (new-pass (student-from-name name)
                                     (make-pass :type 'M
                                                :start-date (time-now)
                                                :amt (parse-integer amt))))
        ((equalp pass "E") (new-pass (student-from-name name)
                                     (make-pass :type 'E
                                                :start-date (time-now)
                                                :amt (parse-integer amt))))
        ((equalp pass "W") (new-pass (student-from-name name)
                                     (make-pass :type 'W
                                                :start-date (time-now)
                                                :amt (parse-integer amt))))
        ((equalp pass "D") (new-drop-in (student-from-name name)
                                        (make-drop-in :date (time-now)
                                                      :amt (parse-integer amt))))
        (t nil))
  (redirect "/main"))
