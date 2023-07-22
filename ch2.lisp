(in-package :lol)

(defun example-program-listing ()
  '(this is
    (a (program (listing)))))

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons
                              (subseq source 0 n)
                              acc))
                   (nreverse
                    (cons source acc))))))
    (if source (rec source nil) nil)))

;;alexandria's flatten takes precedence
(defun flatten-lol (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec
                       (car x)
                       (rec (cdr x) acc))))))
    (rec x nil)))

;;removing the defvar causes temp-special not to be captured via lexical scope
;;i.e. the third value of temp-special-returner-test is 2
(defvar temp-special)
(setq temp-special 1)

(defun temp-special-returner ()
  temp-special)

(defun temp-special-returner-test ()
  (alexandria:flatten (list
   (temp-special-returner)
   (let ((temp-special 2))
     (list temp-special
     (temp-special-returner))))))

;;adapted from https://cl-cookbook.sourceforge.net/dates_and_times.html#intern
(defmacro timing (&body forms)
    (let 
	    ((run1 (gensym))
	     (run2 (gensym))
       (res (gensym)))
    `(let* ((,run1 (get-internal-run-time))
	          (,res (progn ,@forms))
	          (,run2 (get-internal-run-time)))
       (declare (ignore ,res))
	     (/ (- ,run2 ,run1) internal-time-units-per-second))))
(defun register-allocated-fixnum-unoptimized ()
  (let ((acc 0))
    (loop for i from 1 to 100000 do
      (incf acc (the fixnum i)))
    acc))
(defun register-allocated-fixnum ()
  (declare (optimize (speed 3) (safety 0)))
  (let ((acc 0))
    (loop for i from 1 to 100000 do
      (incf (the fixnum acc)
            (the fixnum i)))
    acc))
(defmacro average-timing (&body forms)
  `(/ (iter (for i from 0 to 100000) (sum (timing ,@forms))) 100000))


;;about a 5x speedup
(defun register-allocated-fixnum-test ()
  (list (average-timing (register-allocated-fixnum))
        (average-timing (register-allocated-fixnum-unoptimized))))

(defun let-over-lambda-returner ()
  (let ((y 1))
    (lambda (x)
      (incf y x))))

(defun let-over-lambda-returner-test ()
  (let ((lam (let-over-lambda-returner))
        (lam2 (let-over-lambda-returner)))
    (list (funcall lam 1) (funcall lam 2) (funcall lam2 1))))
;; result: (2 4 2)
;; capturing the let pointer in let-over-lambda-returner ties the life of the
;; underlying pointer to the lambda function in let-over-lambda-returner-test
;; this is more like C's concept of static variables in functions
;; than the examples in the book, except C doesn't allow you to create functions
;; as variables, if let-over-lambda-returner could only return a singleton,
;; it would be effectively identical to C's static functions

(defun block-scanner (trigger-string)
  (let* ((trig (coerce trigger-string 'list))
         (curr trig))
    (lambda (data-string)
      (let ((data (coerce data-string 'list)))
        (dolist (c data)
          ;(format 't "~s ~s~%" c (coerce curr 'string))
          (if curr
              (setq curr
                    (if (char= (car curr) c)
                        (cdr curr)
                        (if (char= c (car trig)) ;this fixes duplicate prefixes
                            (cdr trig)
                            trig)))))
        (not curr)))))

(defun block-scanner-test ()
  (let ((scanner (block-scanner "jihad"))
        (scanner2 (block-scanner "jihad")))
    (values
     (list
      (funcall scanner "We will start ")
      (funcall scanner "the jiji")
      (funcall scanner "had tomorrow."))
     (list
      (funcall scanner2 "We will start ")
      (funcall scanner2 "the ji")
      (funcall scanner2 "had tomorrow.")))))
;(nil nil t)

(let ((direction 'up))
  (defun toggle-counter-direction ()
    (setq direction
          (if (eq direction 'up)
              'down
              'up)))
  (defun counter-class ()
    (let ((counter 0))
      (lambda ()
        (if (eq direction 'up)
            (incf counter)
            (decf counter))))))

(defun counter-class-test ()
  (let ((ct (counter-class)))
    (list (funcall ct) (funcall ct) (toggle-counter-direction) (funcall ct) (funcall ct))))
