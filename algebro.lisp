(defun square (x)
  (* x x))
(assert (equal (square 3) 9))

(defun append-reverses (x)
  (append x (mapcar #'reverse x)))

(defvar *inverses*
  (append-reverses
    '((+ -)
      (* /)
      (square sqrt)
      (sin asin)
      (cos acos)
      (tan atan))))

(defun inverse-op (operator)
  "Returns the inverse operation of OPERATOR"
  (second (assoc operator *inverses*)))
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

(defun op (x)
  "Returns the function name of an expression in Polish
  notation"
  (first x))
(assert (equal (op '(+ 1 2)) '+))

(defun communitivep (x)
  "Returns T if a function follows the communitive property"
  (member (op x) '(+ * =)))
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
(assert (equal (comparison 'tsymbolp) 'string-lessp))

(defun term-type (x)
  "Returns the first predicate that X passes in
  *TERM-ORDER*"
  (find-if
    #'(lambda (op) (funcall op x))
    (mapcar #'first *term-order*)))
(assert (equal (term-type 'a) 'tsymbolp))

(defun less-equationp (x y)
  "Returns T if X is before Y in a mathematical equation"
  (let ((xtype (term-type x))
	(ytype (term-type y)))
    (cond ((equal xtype ytype) (funcall (comparison xtype) x y))
	   (t (beforep xtype ytype (mapcar #'first *term-order*))))))
(assert (less-equationp '2 'a))
(assert (not (less-equationp 'a 2)))
(assert (less-equationp 'a '(b)))
(assert (not (less-equationp '() 'a)))

(defun equation-sort (x)
  "Put variables in alphabetical order, in accordance with
  communitive properties"
  (if (communitivep x)
    (cons (op x) (sort (arguments x) #'less-equationp))
    x))

(defun normalize (x)
  "Recursively put variables in numbers < variables < lists
  order in accordance with communitive properties"
  (cond ((atom x) x)
	(t (equation-sort (mapcar #'normalize x)))))

(defun flatten (tree)
  "Flattens atoms of TREE"
  (cond ((atom tree) (list tree))
	(t (append (flatten (car tree))
		   (flatten (cdr tree))))))

(defun repeated-additionp (expression)
  "If EXPRESSION is a repeated sequence of adding the same
  number, returns the number of times the addend repeats in
  the expression."
  (let* ((args (arguments expression))
	 (addend-count (count (first args) args)))
    (if (equal addend-count (length args))
      addend-count
      nil)))

(defun simplify-repeated-addition (expression)
  (let ((addend-count (repeated-additionp expression)))
    (if addend-count
      (list '* addend-count (first (arguments expression)))
      expression)))
(assert (equal (simplify-repeated-addition '(+ a a a)) '(* 3 a)))

;(defun inverse-addition (fn)
;  (list (inverse-op (op (right fn))) (left fn)
