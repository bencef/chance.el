;;;; A library working with probability distributions.
;;;;
;;;; An example usage:
;;;;
;;;; Chance of going to jail in monopoly because of throwing three
;;;; doubles.
;;;;
;;;; (ch/print
;;;;  (ch/let! ((d1 (ch/d 6))
;;;;            (d2 (ch/d 6))
;;;;            (d3 (ch/d 6))
;;;;            (d4 (ch/d 6))
;;;;            (d5 (ch/d 6))
;;;;            (d6 (ch/d 6)))
;;;;    (ch/pure (and (= d1 d2)
;;;;                  (= d3 d4)
;;;;                  (= d5 d6)))))
;;;;
;;;; ;; t -> 1/216
;;;; ;; nil -> 215/216
;;;; ;; nil
;;;;
;;;; Chance of randomly selected two arrows are not cursed from a quiver
;;;; of ten when five of them are cursed.  From https://xkcd.com/3015/
;;;;
;;;; (ch/print
;;;;  (ch/let! ((arrow1 (ch/events `(normal . ,(ch/make-rational 5 10)) 'cursed))
;;;;            (arrow2 (cl-case arrow1
;;;;                      ((normal) (ch/events `(normal . ,(ch/make-rational 4 9)) 'cursed))
;;;;                      (otherwise (ch/events `(normal . ,(ch/make-rational 5 9)) 'cursed)))))
;;;;   (ch/pure (if (and (eq arrow1 'normal)
;;;;                     (eq arrow2 'normal))
;;;;                'not-cursed 'cursed))))
;;;;
;;;; ;; not-cursed -> 2/9
;;;; ;; cursed -> 7/9
;;;; ;; nil
;;;;
;;;; And the suggested dice throw:
;;;;
;;;; (ch/print
;;;;  (ch/let! ((d1 (ch/d 6))
;;;;            (d2 (ch/d 6))
;;;;            (d3 (ch/d 6))
;;;;            (d4 (ch/d 4)))
;;;;    (ch/pure (if (<= 16 (+ d1 d2 d3 d4))
;;;;                 'not-cursed 'cursed))))
;;;;
;;;; ;; cursed -> 7/9
;;;; ;; not-cursed -> 2/9
;;;; ;; nil

;;; Constructor for rationals

(defun ch/make-rational (n d)
  "Creates a rational number with numerator `n' and denominator `d'"
  (cons n d))

;;; Various helpers for rationals.  Intended to be private.

(defvar ch/--zero (ch/make-rational 0 1))
(defvar ch/--one  (ch/make-rational 1 1))

(defun ch/--rat-+ (&rest rats) ;; RATS!!!
  (let ((lcm (apply #'cl-lcm (mapcar #'cdr rats))))
    (cl-labels
        ((get-n (rat)
           (cl-destructuring-bind (n . d) rat
             (* n (/ lcm d)))))
      (let* ((ns (mapcar #'get-n rats))
             (n (apply #'+ ns)))
        (ch/make-rational n lcm)))))

(defun ch/--rat-* (a b)
  (cl-destructuring-bind
      ((n1 . d1) . (n2 . d2))
      (cons a b)
    (let* ((gcd-n1-d2 (cl-gcd n1 d2))
           (gcd-n2-d1 (cl-gcd n2 d1))
           (n1 (/ n1 gcd-n1-d2))
           (n2 (/ n2 gcd-n2-d1))
           (d1 (/ d1 gcd-n2-d1))
           (d2 (/ d2 gcd-n1-d2)))
      (ch/make-rational (* n1 n2) (* d1 d2)))))

(defun ch/--update-event (value chance m)
  (let ((acc (gethash value m ch/--zero)))
          (puthash value (ch/--rat-+ acc chance) m)))

(defun ch/--rat-complement-1 (rat)
  (cl-destructuring-bind (n . d) rat
    (ch/make-rational (- d n) d)))

(defun ch/--rat-simplify (rat)
  (cl-destructuring-bind (n . d) rat
    (let ((gcd (cl-gcd n d)))
      (ch/make-rational (/ n gcd) (/ d gcd)))))

(defun ch/--rational-p (x)
  (and (consp x)
       (numberp (car x))
       (numberp (cdr x))))

(defun ch/--print-rational (rat)
  (cl-destructuring-bind (n . d) (ch/--rat-simplify rat)
    (format "%d/%d" n d)))

;;; The public functions of the library

(defun ch/pure (v)
  "Create a distribution with a single event that is 100% certain
to happen."
  (let ((m (make-hash-table :size 1)))
    (puthash v ch/--one m)
    m))

(defun ch/--extract-test-fn (args)
  "Extract the test function from the argument list.
This is for simulating a `:test' keyword argument.

	(cl-destructuring-bind
	    (test-fn . args) (ch/--extract-test-fn (list 'a :test 'eq 'b))
	  (list test-fn args))

	;; (eq (a b))"
  (let ((test-fn 'eql)
        (rest (list)))
    (cl-do ((tail args))
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
  "Create a probability distribution where every event has the same chance
of happening.  Duplicates are allowed and are counted more than once.
For example:

	(let ((monty-hall-first-choice (ch/same 'win 'lose 'lose)))
	  (ch/print monty-hall-first-choice))

	;; win -> 1/3
	;; lose -> 2/3
	;; nil

This function also takes a `:test' argument like so:

	(ch/print
	  (ch/same (cons 1 2) (cons 1 2)))

	;; (1 . 2) -> 1/2
	;; (1 . 2) -> 1/2
	;; nil

	(ch/print
	  (ch/same :test 'equal (cons 1 2) (cons 1 2)))

	;; (1 . 2) -> 1/1
	;; nil

	(ch/print
	  (ch/same (cons 1 2) (cons 1 2) :test 'equal))

	;; (1 . 2) -> 1/1
	;; nil"
  (cl-destructuring-bind (test-fn . values) (ch/--extract-test-fn values)
    (let* ((size (length values))
           (m (make-hash-table :size size :test test-fn))
           (chance (ch/make-rational 1 size)))
      (dolist (value values m)
        (ch/--update-event value chance m)))))

(defun ch/events (&rest pairs)
  "Creare a probability distribution where the chance for a given event is
provided.  Event / chance pairs are provided as cons cells.  Events
without chance evenly fill up the rest of the distribution.

	(ch/print
	  (ch/events
	    `(a . ,(ch/make-rational 1 10))
	    'b
	    `(c . ,(ch/make-rational 2 10))
	    'd))

	;; a -> 1/10
	;; c -> 1/5
	;; b -> 7/20
	;; d -> 7/20
	;; nil

This function takes a `:test' keyword argument.  See the documentation of
the `ch/same' function.

There are infinite ways that this can go wrong and none of them are checked:
- Sum over 1/1
- Events represented as cons cells with a rational in cdr
- The same event with and without an explicit chance
- Etc."
  (cl-destructuring-bind (test-fn . pairs) (ch/--extract-test-fn pairs)
    (cl-labels ((has-chance (x) (and (consp x) (ch/--rational-p (cdr x))))
                (standalone (x) (not (has-chance x))))
      (let ((with-chance (cl-remove-if-not #'has-chance pairs))
            (without-chance (cl-remove-if-not #'standalone pairs))
            (acc ch/--zero)
            (m (make-hash-table :size (length pairs))))
        ;; Collect events with explicit chances
        (cl-loop for (e . c) in with-chance
                 do (progn (setq acc (ch/--rat-+ acc c))
                           (ch/--update-event e c m)))
        ;; All remaining events have the same chance
        (when (> (length without-chance) 0)
          (let ((c (ch/--rat-* (ch/--rat-complement-1 acc)
                               (ch/make-rational 1 (length without-chance)))))
            (dolist (e without-chance)
              (ch/--update-event e c m))))
        m))))

(defun ch/map (f v)
  "Apply transform `f' to the events in `v'.  This might change the number
of events.  See:

	(ch/print (ch/map #'cl-oddp (ch/same 1 2 3 4 5)))

	;; t -> 3/5
	;; nil -> 2/5
	;; nil"
  (let ((m (make-hash-table)))
    (maphash #'(lambda (k v)
                 (let ((val (funcall f k)))
                   (ch/--update-event val v m)))
             v)
    m))

(defun ch/d (sides)
  "Simulate a dice throw.  For example `(ch/d 6)' represents throwing a
six-sided die."
  (apply #'ch/same
         (cl-loop for i from 1 to sides
                  collect i)))

(defun ch/bind (ma mf &rest keyword-args)
  "Monadic bind.  Applies `mf' to all events in `ma' and combines the
resulting events under a single distribution.

Example:
After a coin flip if it was tails it can be rethrown once:

	(let ((toss (ch/same 'heads 'tails)))
	  (cl-labels ((maybe-rethrow (last-throw)
	                             (if (eq last-throw 'tails)
	                                 toss
	                               (ch/pure last-throw))))
	    (ch/print
	     (ch/bind toss #'maybe-rethrow))))

	;; heads -> 3/4
	;; tails -> 1/4
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
              (ch/--update-event k1 (ch/--rat-* v v1) m))
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

Multiple binding are possible and the form of a binding can refer to a
previous value:

Throwing with a six sided dice.  If the result is bigger than 3 then
cast another six sided die, otherwise cast a 20 sided die.  What's the
chance that the second throw is bigger than 3?

	(ch/print
	 (ch/let! ((d1 (ch/d 6))
	           (ds (if (> d1 3) (ch/d 6) (ch/d 20))))
	   (ch/pure (> ds 3))))

	;; nil -> 13/40
	;; t -> 27/40
	;; nil

Bindings can contain a `:test' keyword argument which is passed along to
`ch/bind'

	(ch/let! ((val form :test eq))
	  E[val])

NOTE: no need for quoting the test function."
  (if (or (not (listp bindings))
           (not (cl-every #'listp bindings)))
      (error "bindings must be a list of pairs")
    (if (null bindings)
        `(progn ,@body)
      (cl-destructuring-bind (test-fn . first-binding)
          (ch/--extract-test-fn (cl-first bindings))
        (let ((var (cl-first first-binding))
              (ma (cl-second first-binding)))
          `(ch/bind ,ma #'(lambda (,var)
                            (ch/let! ,(cl-rest bindings) ,@body))
                    :test ',test-fn))))))

(defun ch/print (m)
  "Print a probability distribution."
  (maphash
   #'(lambda (k v)
       (princ (format "%s -> %s\n"
                      (prin1-to-string k)
                      (ch/--print-rational v))))
   m))

(provide 'chance)
