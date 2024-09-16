(let ((a 1)
	  (b 3)
	  (c 3)
	  (d 7))
  1
  2
  3
  (print (format "a: %d" a)))

(defun +1 (x)
  (+ 1 x))

(defun foo (x y)
  (setq x 7)
  (+ x y))

(defun ident (x y z u v) x)

(defun fibonacci (x)
  (if (= x 0)
	  1
	  (if (= x 1)
		  1
		  (+ (fibonacci (- x 1)) (fibonacci (- x 2))))))

(defun compiles (x)
  (if x
	  'foo
	  0))

(defun sum (x)
  (if (eq x 'nil)
      0
    (+ (car x) (sum (cdr x)))))

(defun dolist (x f)
  (while x
	(funcall f (car x))
	(setq x (cdr x))))

(defun fold (x s f)
  (let ((res s))
	(dolist x f)))

(setq x 3)
(setq y 5)
(print (format "(foo (let ((x 1) (y 2)) x) 2): %d" (foo (let ((x 1) (y 2)) x) 2)))
(print (format "(ident 1 2 3 4 5): %d" (ident 1 2 3 4 5)))

(print (format "(fibonacci 5): %d" (fibonacci 5)))

(print (format "sum: %d" (sum '(1 2 3 4 5))))

(setq x '("foo" "bar" "baz"))
(setf (cdr x) '(x))

(while x
  (print (format "while: %s" (car x)))
  (setq x (cdr x)))

(defun doprint (x)
  (print (format "doprint: %s" x)))

(dolist '(1 2 3 (4 5)) 'doprint)
