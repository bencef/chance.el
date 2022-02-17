(defun ch/pure (v)
  (let ((m (make-hash-table :size 1)))
    (puthash v 1.0 m)
    m))

(defun ch/same (&rest values)
  (let* ((size (length values))
         (m (make-hash-table :size size)))
    (dolist (value values m)
      (let ((acc (gethash value m 0.0)))
        (puthash value (+ acc (/ 1.0 (float size))) m)))))

(defun ch/map (f v)
  (let ((m (make-hash-table)))
    (maphash #'(lambda (k v)
                 (let* ((val (funcall f k))
                        (old-chance (gethash val m 0.0)))
                   (puthash val (+ old-chance v) m)))
             v)
    m))

(defun ch/d (sides)
  (apply #'ch/same
         (cl-loop for i from 1 to sides
                  collect i)))

(defun ch/bind (ma mf)
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
  (maphash
   #'(lambda (k v)
       (princ (format "%s -> %f\n"
                      (prin1-to-string k)
                      v)))
   m))

(provide 'chance)

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
