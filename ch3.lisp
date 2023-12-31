(in-package lol)
(named-readtables:in-readtable :fare-quasiquote)

(defmacro unit-of-time (value unit)
  `(* ,value ,(case unit
    ((s) 1)
    ((m) 60)
    ((h) 3600)
    ((d) 86400)
    ((ms) 1/1000)
    ((us) 1/1000000))))

(defmacro sleep-units (value unit)
  `(sleep (unit-of-time ,value ,unit)))

(defmacro nlet (n letargs &rest body)
  `(labels ((,n ,(mapcar #'car letargs)
              ,@body))
     (,n ,@(mapcar #'cadr letargs))))

(defun nlet-fact (n)
  (nlet fact ((n n))
         (if (zerop n)
             1
             (* n (fact (- n 1))))))
;;nlet kinda sucks, but the quote "Common Lisp programmers like to write programs" is fun

(defun g!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
                  "G!"
                  :start1 0
                  :end1 2)))

(defmacro defmacro/g! (name args &rest body)
  (let ((syms (remove-duplicates
               (remove-if-not #'g!-symbol-p
                              (flatten body)))))
    (multiple-value-bind (body declarations docstring)
        (parse-body body :documentation t)
      `(defmacro ,name ,args
         ,@(when docstring
             (list docstring))
         ,@declarations
         (let ,(mapcar
                (lambda (s)
                  `(,s (gensym ,(subseq
                                 (symbol-name s)
                                 2))))
                syms)
           ,@body)))))

(defmacro/g! example-macro (a b)
  `(let ((,G!temp (+ ,a ,b)))
     (* 2 ,G!temp)))

(defmacro/g! nif (expr pos neg zero)
  `(let ((,g!res ,expr))
     (cond ((plusp ,g!res) ,pos)
           ((zerop ,g!res) ,zero)
           (t ,neg))))

(named-readtables:in-readtable :standard)
