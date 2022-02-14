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

(defun inverse-op (op)
  "Returns the inverse operation of OP"
  (second (assoc op *inverses*)))
