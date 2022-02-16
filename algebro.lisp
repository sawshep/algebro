(defun square (x)
  (* x x))
(assert (equal (square 3) 9))

(defvar *inverses*
  '((+ . -)
;    (- . +)
    (* . /)
;    (/ . *)
    (sqrt . square)))
;    (square sqrt)))

;(defun inverse-op (operator)
;  "Returns the inverse operation of OPERATOR"
;  (second (assoc operator *inverses*)))

(defun inverse-op (operation)
  "Returns the inverse algebraic operation of OPERATION"
  (or (cdr (assoc operation *inverses*))
      (car (rassoc operation *inverses*))))
(assert (equal (inverse-op '+) '-))
(assert (equal (inverse-op '-) '+))

(defvar *assertions*
  '((f (/ (* m (square v)) 2))))

(defun reciporical (x)
  (/ 1.0 x))

;;; Double-condition tail recursion
(defun deep-member (x root)
  "Returns T if TREE contains X at any depth"
  (labels ((sub (child)
             (cond ((member x child) t)
                   ((not (find-if #'consp child)) nil)
                   (t (find-if #'sub (remove-if-not #'consp child))))))

    (sub root)))
(assert (deep-member 'foo '(a (b (c d foo e) f (x y) g) hi)))

(defun left (equation)
  "Returns the left side of an equation"
  (first equation))
(assert (equal (left '(x (+ x 0))) 'x))

(defun right (equation)
  "Returns the right side of an equation"
  (second equation))
(assert (equal (right '(x (+ x 0))) '(+ x 0)))

(defun arguments (fn)
  (rest fn))
(assert (equal (arguments '(+ 1 2)) '(1 2)))

(defun fn (x)
  "Returns the function name of an expression in Polish
  notation"
  (first x))
(assert (equal (fn '(+ 1 2)) '+))

(defun communitivep (x)
  "Returns T if a function follows the communitive property"
  (member (fn x) '(+ *)))
(assert (communitivep '(+ 1 2)))
(assert (not (communitivep '(/ 1 2))))

(defun beforep (x y l)
  "Returns T if X appears before Y in L"
  (member y (member x l)))
(assert (beforep 'foo 'bar '(foo bar)))
(assert (not (beforep 'bar 'foo '(foo bar))))

(defun list-lessp (x y)
  "Returns T if the length of X is less than the length of
  Y"
  (if x (< (length x) (length y))))
(assert (list-lessp '(1) '(1 2)))
(assert (not (list-lessp '(1 2) '(1))))

(defun tsymbolp (x)
  "Returns T if X is both not NIL and a symbol"
  (and x (symbolp x)))
(assert (tsymbolp 'a))
(assert (not (tsymbolp nil)))

;;; In an equation, numbers go before variables go before
;;; lists.
(defvar *term-order*
  '((numberp >)
    (tsymbolp string-lessp)
    (listp list-lessp)))

(defun comparison (x)
  "Returns the less presdicate to use for simple term type
  X"
  (second (assoc x *term-order*)))

(defun term-type (x)
  "Returns the first predicate that X passes in
  *TERM-ORDER*"
  (find-if
    #'(lambda (fn) (funcall fn x))
    (mapcar #'first *term-order*)))

(defun equationsortp (x y)
  "Returns T if X is before Y in a mathematical equation"
  (let ((xtype (term-type x))
	(ytype (term-type y)))
    (cond ((equal xtype ytype) (funcall (comparison xtype) x y))
	   (t (beforep xtype ytype (mapcar #'first *term-order*))))))

(defun normalize-aux (x)
  "Put variables in alphabetical order, in accordance with
  communitive properties"
  (if (communitivep x)
    (cons (fn x) (sort (arguments x) #'equationsortp))
    x))

;(defun normalize (x)
;  (cond ((atom x) x)
;	((not (communitive x) (mapcar #'normalize x)))
;	((t mapcar #'normalize x))))

(defun normalize (x)
  (cond ((atom x) x)
	((communitivep x) (normalize-aux (mapcar #'normalize x)))
	((listp x) (mapcar #'normalize x))))

(defun flatten (tree)
  "Flattens atoms of TREE"
  (cond ((atom tree) (list tree))
	(t (append (flatten (car tree))
		   (flatten (cdr tree))))))

