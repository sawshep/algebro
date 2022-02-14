(defun square (x)
  (* x x))

(defvar *inverses*
  '((+ -)
    (- +)
    (* /)
    (/ *)
    (sqrt square)
    (square sqrt)))

(defvar *assertions*
  '((f (/ (* m (square v)) 2))))

(defun reciporical (x)
  (/ 1.0 x))

;;; Double-condition tail recursion
(defun deep-member (x root)
  "Returns T if TREE contains X at any point"
  (labels ((sub (child)
             (cond ((member x child) t)
                   ((not (find-if #'consp child)) nil)
                   (t (find-if #'sub (remove-if-not #'consp child))))))

    (sub root)))

(defun left (equation)
  "Returns the left side of an equation"
  (first equation))

(defun right (equation)
  "Returns the right side of an equation"
  (second equation))

(defun arguments (fn)
  (rest fn))

(defun fn-name (fn)
  (first fn))

(defun communitivep (x)
  "Returns T if a function follows the communitive property"
  (member (fn-name x) '(+ *)))

(defun beforep (x y l)
  "Returns T if X appears before Y in L"
  (member y (member x l)))

;(defun listbeforep (x y)
;  (labels first-atom (l)
;    (cond ((atom (car l) (car l))
;	   (t (first-atom (find-if #'listp l))))))
;  (> (first-atom x) (first-atom y)))

(defun listbeforep (x y)
  (< (length x) (length y)))

(defun equationsortp (x y)
	(cond ((and (numberp x) (numberp y) (> x y))
	       (and ()))))

(defun normalize (x)
  "Put variables in alphabetical order, in accordance with
  communative and associative properties"
  (cond (())))

(defun flatten (depth x)
  (cond ((atom tree) (list tree))
	(t (append (flatten (car tree))
		   (flatten (cdr tree))))))

(defun inverse-op (op)
  "Returns the inverse operation of OP"
  (second (assoc op *inverses*)))


