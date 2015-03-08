(in-package #:shala-sys)

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

;;;  Parenscript macro for showModal() and close() methods for pop-up dialogs.
;;;Takes the dialog's id, button for opening the dialog's id, and closing button's id.
(ps (defmacro open-close-modal-dialog (dialog-id element-id-1 element-id-2 &key open close open-args close-args)
      (let ((dialog (ps-gensym)))
        `(progn
           (setf ,dialog (chain document (get-element-by-id ,dialog-id)))
           (setf (chain document (get-element-by-id ,element-id-1) onclick)
                 (lambda (,@open-args)
                   (progn
                     ,@open
                     (funcall (chain ,dialog show-modal)))))
           (setf (chain document (get-element-by-id ,element-id-2) onclick)
                 (lambda (,@close-args)
                   (progn
                     ,@close
                     (funcall (chain ,dialog close)))))))))

;;; Converts lisp property-list to JSON
(defun pass->json (pass)
  (let ((new-pass (copy-list pass)))
    (setf (getf new-pass :date) (print-year-month-day (getf new-pass :date)))
    (cl-json:encode-json-plist-to-string new-pass)))

;;;HTML Views

;;; Standard Page template
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

;;; Main page displaying students that attended class on that day
(define-easy-handler (main :uri "/main") ()
  (standard-page (:title "Ashtanga Yoga Osaka"
                         ;; Shows the pop-up dialog with the list of students
                         :script (ps
                                   (defun init ()
                                     (open-close-modal-dialog "addStudentDialog" "addStudent" "closeList")
                                     (open-close-modal-dialog "newStudentDialog" "newStudent" "closeNewStudent"))
                                   (setf (chain window onload) init)))
    
    (:table :class "maintable"
     (:caption "Students Today")
     (:col)
     (:tbody ;Displays list of students that attended today
      (:tr (:th "Name")
           (:th "Pass"))
      (dolist (student (reverse *students-today*)) ;Shows the students in the order they signed in
        (htm
         (:tr
          (:td (:a :class "cancelButton" :href (format nil "/remove-today?name=~A" (name student)) "X")
               (fmt "~A" (name student)))
          (cond ((equal (validate-drop-in student) t) (htm (:td (fmt "Drop-in")))) ; Check if Drop-in or not
                (t (htm (:td (fmt "~A" (get-type (pass-of student))))))))))))

    ;; Pop-up dialog with list of students to choose from
    (:dialog :id "addStudentDialog"
             (:div :class "buttonLinkList"
                   (dolist (student (students))
                     (htm
                      (:a :class "modalList" :href (format nil "/validate-and-add?name=~A" (name student))
                          (fmt "~A" (name student)))
                      (:br))))
             (:p (:a :href "#" :id "newStudent" :class "btn" "New Student")
                 (:a :href "#" :id "closeList" :class "btn" "Exit")))

    ;; Pop-up dialog for registering new students
    (:dialog :id "newStudentDialog"
             (:h1 "Register New Student")
             (:form :action "/register-new-student" :method "post" :id "register-student"
                    (:p "Name" (:input :type "text" :name "name" :class "txt"))
                    (:p "Email" (:input :type "email" :name "email" :class "txt"))
                    (:p (:input :type "submit" :value "Register" :class "btn")
                        (:a :class "btn" :href "#" :id "closeNewStudent" "Cancel"))))

    (:div :id "actionlist"
          (:a :class "btn" :href "#" :id "addStudent"  "Add Student")
          (:a :class "btn" :href "student-list" "Student List")
          (:a :class "btn" :href "reports" "Reports"))))

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
                 ;; Edit student info on click 
                 (:td (:a :href (format nil "/student-info?name=~A" (name student)) :class "listLink" (fmt "~A" (name student))))
                 (:td (fmt "~A" (email student)))
                 (:td (fmt "~A" (cond ((pass-p student) ; retrieve the latest pass details if existent
                                       (print-month-day (get-record-date (pass-of student))))
                                      (t (pass student))))))))))
    (:div :id "actionlist"
          (:a :class "btn" :href "main" "Main"))))

;;;  Complicated, tedious, ugly-ass code to make the pop-up editPassDialog dialog display the correct information from the selected
;;;pass and to pass it back correctly to the editStudent form. The editStudent form converts the pass to JSON, which is then picked
;;;up by the page's Javascript and passed on to the editPassDialog once the onclick() event fires. Once the dialog's submit button
;;;is clicked, the current values in the fields are then used to create a new JSON structure, copying the old one, with the new field
;;;values. This is then used to replace the old one residing in form.passlist.selectedIndex.
(define-easy-handler (student-name :uri "/student-info") (name)
  (let ((student (student-from-name name)))
    (standard-page (:title "Ashtanga Yoga Osaka | Student Page"
                           :script (ps
                                     (defun init ()
                                       (open-close-modal-dialog "editPassDialog" "getPass" "submitPass"
                                                                       ;; This is the pop-up dialog
                                                                :open ((defvar frm (chain document (get-element-by-id "editPass")))
                                                                       ;; The main form
                                                                       (defvar form (chain document forms (named-item "editStudent")))
                                                                       ;; Binds the current selected option from the drop-down list of passes
                                                                       (defvar pass (chain *json* (parse (chain form passlist options[form passlist selected-index] value))))
                                                                       ;;  Replacing the default values of the pop-up dialog with the current ones from the main form. Values starting with "o-" are the original values, they are retained so that the original pass can be recreated and used with (position) to find the index of the pass and replace it with the new pass.
                                                                       (setf (chain frm "o-name" value) 
                                                                             (chain form name value)) 
                                                                       (setf (chain frm date value)
                                                                             (chain pass date))
                                                                       (setf (chain frm "o-date" value) 
                                                                             (chain pass date))
                                                                       (setf (chain frm type value)
                                                                             (chain pass type))
                                                                       (setf (chain frm "o-type" value) 
                                                                             (chain pass type))
                                                                       (setf (chain frm amt value)
                                                                             (chain pass amt))
                                                                       (setf (chain frm "o-amt" value)
                                                                             (chain pass amt))))
                                       (open-close-modal-dialog "editPassDialog" "newPass" "submitPass"
                                                                :open ((defvar frm (chain document (get-element-by-id "editPass")))
                                                                       (defvar form (chain document forms (named-item "editStudent")))
                                                                       (setf (chain frm "o-name" value)
                                                                             (chain form name value)))))
                                     (setf (chain window onload) init)))
                   ;; Main form
                   (:div :class "horizCenterForm"
                         (:form :action "/edit-student" :method "post" :id "editStudent"
                                (:p "Name" (:input :type "text" :name "name" :class "txt" :value (format nil "~A" (name student))))
                                (:P "Email" (:input :type "email" :name "email" :class "txt" :value (format nil "~A" (email student))))
                                (:p "Passes" (:select :name "passlist" 
                                                      (dolist (pass (pass student))
                                                        (htm
                                                         (:option :id "pass" :value (pass->json pass)
                                                                  (fmt "~A ~A" (print-month (getf pass :date))
                                                                       (print-year (getf pass :date)))))))
                                    (:button :type "button" :id "getPass" :class "btn" "Get Pass")
                                    (:button :type "button" :id "newPass" :class "btn" "Add Pass"))
                                (:input :type "hidden" :name "old-name" :value name) ; old name of student, used for retrieving the correct instance
                                (:p (:input :type "submit" :value "Edit Info" :class "btn"))))
                   ;; Pop-up dialog for editing passes
                   (:dialog :id "editPassDialog"
                            (:h1 "Add | Edit Pass")
                            (:form :action "/edit-pass" :method "post" :id "editPass"
                                   (:input :type "hidden" :name "o-name" :value nil)
                                   (:input :type "hidden" :name "o-date" :value nil)
                                   (:input :type "hidden" :name "o-type" :value nil)
                                   (:input :type "hidden" :name "o-amt" :value nil)
                                   (:p "Date bought" (:input :type "text" :class "txt" :name "date"))
                                   (dolist (type '(("Morning" . "m") ("Evening" . "e") ("Week" . "w")))
                                     (htm
                                      (:p (:input :type "radio" :name "type" :value (rest type) :id (rest type)) (fmt "~A" (first type)))))
                                   (:p "Amount Paid" (:input :type "text" :name "amt"))
                                   (:p "Delete Pass?" (:input :type "checkbox" :name "del"))
                                   (:p (:button :type "submit" :class "btn" :id "submitPass" "Confirm"))))
                   (:div :id "actionlist"
                         (:a :class "btn" :href "main" "Main")
                         (:a :class "btn" :href "student-list" "Student List")))))

(define-easy-handler (edit-pass :uri "/edit-pass") (o-name o-date o-type o-amt date type amt del)
  (if (> (length o-date) 0) ; if length is greater than 0 then a pass was edited, otherwise, a brand new pass was added.
      (if (equalp del "on")
          (remove-pass (student-from-name o-name)
                       (make-pass :type (intern (string-upcase o-type) :shala-sys) :amt (parse-integer o-amt) :date (print-year-month-day->timestamp o-date)))
          (find-and-edit-pass (student-from-name o-name)
                              ;; Recreates old pass from string values passed from the form
                              (make-pass :type (intern (string-upcase o-type) :shala-sys) :amt (parse-integer o-amt) :date (print-year-month-day->timestamp o-date))
                              (pass (student-from-name o-name))
                              ;; New pass created from string values passed from the form
                              (make-pass :date (print-year-month-day->timestamp date) :type (intern (string-upcase type) :shala-sys) :amt (parse-integer amt))))
      (new-pass (student-from-name o-name)
                (make-pass :date (print-year-month-day->timestamp date) :type (intern (string-upcase type) :shala-sys) :amt (parse-integer amt))))
  (redirect (format nil "/student-info?name=~A" o-name)))

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
    (:div :class "horizCenterForm"
          (:h1 "Buy pass or drop-in") 
          (:form :action (format nil "/add-pass-drop-in?name=~A" name) :method "post" :id "buy-pass-drop-in"
                 (:p "Morning Pass" (:input :type "radio" :name "pass" :value 'M))
                 (:p "Evening Pass" (:input :type "radio" :name "pass" :value 'E))
                 (:p "1 Week Pass" (:input :type "radio" :name "pass" :value 'W))
                 (:p "Drop-in" (:input :type "radio" :name "pass" :value 'D))
                 (:p "Amount" (:input :type "number" :name "amt"))
                 (:p (:input :type "submit" :value "Buy" :class "btn"))))))

(define-easy-handler (add-pass-drop-in :uri "/add-pass-drop-in") (name pass amt)
  (cond ((equalp pass "M") (new-pass (student-from-name name)
                                     (make-pass :type 'M
                                                :date (time-now)
                                                :amt (parse-integer amt))))
        ((equalp pass "E") (new-pass (student-from-name name)
                                     (make-pass :type 'E
                                                :date (time-now)
                                                :amt (parse-integer amt))))
        ((equalp pass "W") (new-pass (student-from-name name)
                                     (make-pass :type 'W
                                                :date (time-now)
                                                :amt (parse-integer amt))))
        ((equalp pass "D") (new-drop-in (student-from-name name)
                                        (make-drop-in :date (time-now)
                                                      :amt (parse-integer amt))))
        (t nil))
  (redirect "/main"))
