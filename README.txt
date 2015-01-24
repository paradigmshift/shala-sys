#+TITLE:Shala-sys Documentation 
#+AUTHOR: Mozart Reina
#+email: mozart@mozartreina.com
#+INFOJS_OPT: 
#+BABEL: :session *lisp* :cache yes :results output graphics :export both :tangle yes 
-----

* queries.lisp
** this-month-passes 
   this-month-passes -> =list= =list= =list=

   Returns three lists, all morning, evening, and weekly passes of the current month.
** passes-on 
   passes-on =year= =month= -> =list= =list= =list=
   
   Returns three lists, all morning, evening, and weekly passes valid for the given year and month.
** pass-total-for 
   pass-total-for =year= =month= =student= -> =float=
   
   Returns the prorated amount of the pass that matches the year and month of the given student + whatever carry-over from the previous month's pass. If no pass is present for the given year and month, then just the carry-over is returned.
** current-or-prev-pass-on 
   current-or-prev-pass-on =year= =month= =student= -> =plist=

   Returns the pass that matches the year and month for the given student, otherwise returns the previous month's pass.
   #+BEGIN_SRC lisp
     (current-or-prev-pass-on 2014 12 (student-from-name "yukako kotani"))
     => (:TYPE M :DATE @2014-11-23T20:50:28.000000+09:00 :AMT 17000)
   #+END_SRC
