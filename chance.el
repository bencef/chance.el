;; A library working with probability distributions.
;;
;; An example usage:
;;
;; Chance of going to jail in monopoly because of throwing three
;; doubles.
;;
;; (ch/print
;;  (ch/let! ((d1 (ch/d 6))
;;            (d2 (ch/d 6))
;;            (d3 (ch/d 6))
;;            (d4 (ch/d 6))
;;            (d5 (ch/d 6))
;;            (d6 (ch/d 6)))
;;    (ch/pure (and (= d1 d2)
;;                  (= d3 d4)
;;                  (= d5 d6)))))
;;
;; ;; t -> 0.004630
;; ;; nil -> 0.995370
;; ;; nil

(defun ch/pure (v)
  "Create a distribution with a single event that is 100% certain
to happen."
  (let ((m (make-hash-table :size 1)))
    (puthash v 1.0 m)
    m))

(defun ch/--extract-test-fn (args)
  "Extract the test function from the argument list.
This is for simulating a `:test' keyword argument.

	(destructuring-bind (test-fn . args) (ch/--extract-test-fn (list 'a :test 'eq 'b))
	  (list test-fn args))

	;; (eq (a b))"
  (let ((test-fn 'eql)
        (rest (list)))
    (do ((tail args))
        ((null tail))
      (if (eq :test (car tail))
          (progn
            (setf test-fn (cadr tail))
            (setf tail (cddr tail)))
        (progn
          (push (car tail) rest)
          (setf tail (cdr tail)))))
    (cons test-fn (nreverse rest))))


(defun ch/same (&rest values)
  "Create a probability distribution where every event has the same chance of happening.
Duplicates are allowed and are counted more than once.  For example:

	(let ((monty-hall-first-choice (ch/same 'win 'lose 'lose)))
          (ch/print monty-hall-first-choice))

	;; win -> 0.333333
	;; lose -> 0.666667
	;; nil

This function also takes a `:test' argument like so:

	(ch/print
	 (ch/same (cons 1 2) (cons 1 2)))

	;; (1 . 2) -> 0.500000
	;; (1 . 2) -> 0.500000
	;; nil

	(ch/print
	 (ch/same :test 'equal (cons 1 2) (cons 1 2)))

	;; (1 . 2) -> 1.000000
	;; nil

	(ch/print
	 (ch/same (cons 1 2) (cons 1 2) :test 'equal))

	;; (1 . 2) -> 1.000000
	;; nil"
  (destructuring-bind (test-fn . values) (ch/--extract-test-fn values)
    (let* ((size (length values))
           (m (make-hash-table :size size :test test-fn)))
      (dolist (value values m)
        (let ((acc (gethash value m 0.0)))
          (puthash value (+ acc (/ 1.0 (float size))) m))))))

(defun ch/events (&rest pairs)
  "Creare a probability distribution where the chance for a given event is provided.
Event / chance pairs are provided as cons cells.  Events without chance evenly
fill up the rest of the distribution.

	(ch/print
	 (ch/events `(a . ,0.1) 'b `(c . ,0.2) 'd))

	;; a -> 0.100000
	;; c -> 0.200000
	;; b -> 0.350000
	;; d -> 0.350000
	;; nil

There are infinite ways that this can go wrong and none of them are checked:
- Sum over 1.0
- Events represented as cons cells with a number in cdr
- The same event with and without an explicit chance
- Etc."
  (labels ((has-chance (x) (and (consp x) (typep (cdr x) 'float)))
           (standalone (x) (not (has-chance x))))
    (let ((with-chance (remove-if-not #'has-chance pairs))
          (without-chance (remove-if-not #'standalone pairs))
          (acc 0.0)
          (m (make-hash-table :size (length pairs))))
      ;; Collect events with explicit chances
      (loop for (e . c) in with-chance
            do (progn (incf acc c)
                      (let ((old (gethash e m 0.0)))
                        (puthash e (+ old c) m))))
      ;; All remaining events have the same chance
      (let ((c (/ (- 1.0 acc) (length without-chance))))
        (dolist (e without-chance)
          (let ((old (gethash e m 0.0)))
            (puthash e (+ old c) m))))
      m)))


(defun ch/map (f v)
  "Apply transform `f' to the events in `v'.  This might change the number of events.  See:

	(ch/print (ch/map #'oddp (ch/same 1 2 3 4 5)))

	;; t -> 0.600000
	;; nil -> 0.400000
	;; nil"
  (let ((m (make-hash-table)))
    (maphash #'(lambda (k v)
                 (let* ((val (funcall f k))
                        (old-chance (gethash val m 0.0)))
                   (puthash val (+ old-chance v) m)))
             v)
    m))

(defun ch/d (sides)
  "Simulate a dice throw.  For example `(ch/d 6)' represents throwing a six-sided die."
  (apply #'ch/same
         (cl-loop for i from 1 to sides
                  collect i)))

(defun ch/bind (ma mf &rest keyword-args)
  "Monadic bind.  Applies `mf' to all events in `ma' and combines the resulting events
under a single distribution.

Example:
After a coin flip if it was tails it can be rethrown once:

	(let ((toss (ch/same 'heads 'tails)))
	  (cl-labels ((maybe-rethrow (last-throw)
	                             (if (eq last-throw 'tails)
	                                 toss
	                               (ch/pure last-throw))))
	    (ch/print
	     (ch/bind toss #'maybe-rethrow))))

	;; heads -> 0.750000
	;; tails -> 0.250000
	;; nil

This function accepts a `:test' keyword argument like so:
	(ch/bind val #'fun :test 'eq)"
  (let* ((test-fn (car (ch/--extract-test-fn keyword-args)))
         (s (hash-table-count ma))
         (m (make-hash-table :size (* s s) :test test-fn)))
    (maphash
     #'(lambda (k v)
         (maphash
          #'(lambda (k1 v1)
              (let ((acc (gethash k1 m 0.0)))
                (puthash k1 (+ acc (* v v1)) m)))
          (funcall mf k)))
     ma)
    m))

(put 'ch/let! 'lisp-indent-function 1)

(defmacro ch/let! (bindings &rest body)
  "A monadic let binding.  Similar to do-notation in functional languages.
While a regular let can be thought of as a lambda application:

	(let ((value form))
	  E[value])

is equvalent to:

	(funcall #'(lambda (value) E[value])
	         form)

`ch/let!' is simalar but instead of a `ch/bind' in place of the `funcall'.
(The arguments are filpped but that's an arbitraty choice)

	(ch/let! ((value form))
	  E[value])

is equvalent to:

	(ch/bind form
	         #'(lambda (value) E[value]))

Multiple binding are possible and the form of a binding can refer to a previous value:

Throwing with a six sided dice.  If the result is bigger than 3 then cast another six
sided die, otherwise cast a 20 sided die.  What's the chance that the filpped throw is
bigger than 3?

	(ch/print
	 (ch/let! ((d1 (ch/d 6))
	           (ds (if (> d1 3) (ch/d 6) (ch/d 20))))
	   (ch/pure (> ds 3))))

	;; nil -> 0.325000
	;; t -> 0.675000
	;; nil

Bindings can contain a `:test' keyword argument which is passed along to `ch/bind'

	(ch/let! ((val form :test eq))
	  E[val])

NOTE: no need for quoting the test function."
  (if (or (not (listp bindings))
           (not (every #'listp bindings)))
      (error "bindings must be a list of pairs")
    (if (null bindings)
        `(progn ,@body)
      (destructuring-bind (test-fn . first-binding)
          (ch/--extract-test-fn (first bindings))
        (let ((var (first first-binding))
              (ma (second first-binding)))
          `(ch/bind ,ma #'(lambda (,var)
                            (ch/let! ,(rest bindings) ,@body))
                    :test ',test-fn))))))

(defun ch/print (m)
  "Print a probability distribution."
  (maphash
   #'(lambda (k v)
       (princ (format "%s -> %f\n"
                      (prin1-to-string k)
                      v)))
   m))

(provide 'chance)
