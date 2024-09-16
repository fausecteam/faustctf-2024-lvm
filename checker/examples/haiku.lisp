(setq *random-seed* 42)

(defun random (v)
  (let ((a 17)
        (c 52)
        (m 256))
    (setq *random-seed* (+ (* a *random-seed*) c))
	(while (> *random-seed* m)
      (setq *random-seed* (- *random-seed* m)))
	
    (let ((r *random-seed*))
	  (while (< v r)
		(setq r (- r v)))
	  r)))

(defun random-range (min max)
  (+ min (random (- max min))))

(defun nth (n l)
  (if (= n 0)
	  (car l)
	  (nth (- n 1) (cdr l))))

(defun length (l)
  (if l
	  (+ (length (cdr l)) 1)
	  0))

(defun random-choice (lst)
  (nth (random-range 0 (length lst)) lst))

(defun random-haiku ()
  (let ((line1 (random-line 5))
        (line2 (random-line 7))
        (line3 (random-line 5)))
	(format "%s\n%s\n%s" line1 line2 line3)))

(defun random-line (syllables)
  (let ((line nil)
		(choice nil))
    (while (> syllables 0)
	  (setq choice (random-choice *word-list*))
      (setq line (cons (car choice) line))
      (setq syllables (- syllables (nth 1 choice))))
    (format-line line)))

(defun format-line (line)
  (let ((formatted ""))
    (while line
      (setq formatted (format "%s%s " formatted (car line)))
	  (setq line (cdr line)))
	formatted))

(setq *word-list*
  '(("autumn" 2) ("breeze" 1) ("crimson" 2) ("dewdrops" 2) ("echoes" 2) ("frost" 1) ("golden" 2) ("harvest" 2) ("innocence" 3) ("jade" 1)
    ("kiss" 1) ("luminous" 3) ("mist" 1) ("nightfall" 2) ("ocean" 2) ("petals" 2) ("quiver" 2) ("rivers" 2) ("silence" 2) ("tranquil" 2)
    ("umbrella" 3) ("violet" 3) ("whisper" 2) ("zen" 1)))

(print (random-haiku))
nil
