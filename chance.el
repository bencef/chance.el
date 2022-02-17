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

(defun ch/same (&rest values)
  "Create a probability distribution where every event has the same chance of happening.
Duplicates are allowed and are counted more than once.  For example:

	(let ((monty-hall-first-choice (ch/same 'win 'lose 'lose)))
          (ch/print monty-hall-first-choice))

	;; win -> 0.333333
	;; lose -> 0.666667
	;; nil"
  (let* ((size (length values))
         (m (make-hash-table :size size)))
    (dolist (value values m)
      (let ((acc (gethash value m 0.0)))
        (puthash value (+ acc (/ 1.0 (float size))) m)))))

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

(defun ch/bind (ma mf)
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
	;; nil"
  (let* ((s (hash-table-count ma))
         (m (make-hash-table :size (* s s))))
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
sided die, otherwise cast a 20 sided die.  What's the chance that the final throw is
bigger than 3?

	(ch/print
	 (ch/let! ((d1 (ch/d 6))
	           (ds (if (> d1 3) (ch/d 6) (ch/d 20))))
	   (ch/pure (> ds 3))))

	;; nil -> 0.325000
	;; t -> 0.675000
	;; nil"
  (if (or (not (listp bindings))
           (not (every #'listp bindings)))
      (error "bindings must be a list of pairs")
    (if (null bindings)
        `(progn ,@body)
      (let ((var (first (first bindings)))
            (ma (second (first bindings))))
        `(ch/bind ,ma #'(lambda (,var)
                          (ch/let! ,(rest bindings) ,@body)))))))

(defun ch/print (m)
  "Print a probability distribution."
  (maphash
   #'(lambda (k v)
       (princ (format "%s -> %f\n"
                      (prin1-to-string k)
                      v)))
   m))

(provide 'chance)
