(in-package :ld-music)

(defun note-name-position (note-name &optional (scale (midi-notes)))
  (position note-name scale :test (lambda (x y) (equal x (note-name y)))))

(defun find-note-in-octave (note notes)
  (find-if (lambda (n)
	     (note-equal-p note (car n))) notes))

;; Note selector functions
(defun note-attr (note attr) (cdr (assoc attr note)))
(defun note-name (note) (note-attr note 'name))
(defun note-value (note) (note-attr note 'value))
(defun note-solfege (note) (note-attr note 'solfege))
(defun note-octave (note) (note-attr note 'octave))
(defun note-relative-octave (note)
  (note-attr note 'relative-octave))
(defun note-equal-p (x y)
  (and (equal (note-value x)
	      (note-value y))
       (equal (note-name x)
	      (note-name y))))

(defun note-solfege-equalp (note solfege)
  (if (listp (note-solfege note))
      (member solfege (note-solfege note))
      (equal solfege (note-solfege note))))

(defun note-solfege-member-p (note lofsolfege)
  (if (listp (note-solfege note))
      (some (lambda (s) (member s lofsolfege)) (note-solfege note))
      (member (note-solfege note) lofsolfege)))

(defun note-idx (note &optional (scale (midi-notes)))
  (position note scale :test #'note-equal-p))

(defun note-octave-up (note scale)
  (let* ((other-note (nth (+ 12 (note-idx note)) scale)))
    (when other-note
      (attr= (note-solfege note) 'solfege other-note)
      other-note)))
(defun note-octave-down (note scale)
  (if (>= (- (note-idx note) 12) 0)
      (let* ((other-note (nth (- (note-idx note) 12) scale)))
	(when other-note
	  (attr= (note-solfege note) 'solfege other-note)
	  other-note))))

(defun parse-note-octave (note-name)
  (parse-integer (car (multiple-value-list (cl-ppcre:scan-to-strings "\\d" (symbol-name note-name))))))

(defun find-note (name &optional (scale (midi-notes)))
  (find-if (lambda (note) (eq (note-name note) name)) scale))

(defun make-note (name value solfege)
  (list
   (cons 'type 'note)
   (cons 'name name)
   (cons 'value value)
   (cons 'solfege solfege)
   (cons 'relative-octave nil)
   (cons 'octave (parse-note-octave name))))
