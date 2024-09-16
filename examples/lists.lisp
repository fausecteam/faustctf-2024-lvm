(defun even(n)
  (if (= 0 n)
	  't
	  (not (even (- n 1)))))
(defun odd(n) (not (even n)))

(defun filter (l p)
  (if (eq l 'nil)
	  'nil
	  (if (funcall p (car l))
		  (cons (car l) (filter (cdr l) p))
		  (filter (cdr l) p))))

(print (format "(filter '(6 4 3 5 2) 'even): %s" (filter '(6 4 3 5 2) 'even)))

(defun concat (a b)
  (if a
	  (cons (car a) (concat (cdr a) b))
	  b))

(defun powerset (a)
  (let ((res nil))
	(while a
	  (setq res (cons a res))
	  (if (cdr a)
		  (setq res (cons (cons (car a) nil) res)))
	  (setq a (cdr a)))
	(cons nil res)))

(defun bubblesort (x cmp)
  (if x
	  (if (cdr x)
		  (let ((a (car x))
				(inner (bubblesort (cdr x) cmp))
				(b (car inner))
				(tail (cdr inner)))
			(if (> (funcall cmp a) (funcall cmp b))
				(cons b (bubblesort (cons a tail) cmp))
				(cons (car x) inner)))
		  x)
	  nil))

(defun length (x)
  (let ((len 0))
	(while x
	  (setq len (+ 1 len))
	  (setq x (cdr x)))
	len))

(defun reverse (x output)
  (if x
	  (reverse (cdr x) (cons (car x) output))
	  output))

(defun split-at (x l)
  (if (< l 1)
	  (cons nil x)
	  (let ((split (split-at (cdr x) (- l 1))))
		(cons (cons (car x) (car split)) (cdr split)))))

(defun mergesort--split (x)
  (let ((start x)
		(l (length x)))
	(split-at x (/ l 2))))

(defun mergesort--combine (a b cmp)
  (let ((res nil))
	(while (and a b)
	  (if (< (funcall cmp (car a)) (funcall cmp (car b)))
		  (progn
			(setq res (cons (car a) res))
			(setq a (cdr a)))
		  (setq res (cons (car b) res))
		  (setq b (cdr b))))
	(concat (reverse res nil) (or a b))))

(defun mergesort (x cmp)
  (if (cdr x)
	  (let ((split (mergesort--split x)))
		(mergesort--combine (mergesort (car split) cmp) (mergesort (cdr split) cmp) cmp))
	  x))

(print (format "(concat '(foo bar) '(baz 'nil)): %s" (concat '(foo bar) '(baz 'nil))))
(print (format "(powerset '(1 2 3)): %s" (powerset '(1 2 3))))

(setq list '(3 7 1 2 4 7 10 5))
(print (format "unsorted: %s" list))
(print (format "sorted: %s" (mergesort list (defun identity (x) x))))
