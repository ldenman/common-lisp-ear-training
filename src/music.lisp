(in-package :ld-music)

(defvar *midi-out3* nil)
(setf *midi-out3* nil)

(defun pm-reload ()
  (if *midi-out3*
      (progn
	(let ((oldid *midi-out3*))
	  (setf *midi-out3* nil)
	  (pm:close-midi oldid))))
  (pm:list-devices)
  (pm:terminate)
  (pm:initialize)
  (setf *midi-out3* (pm:open-output 2 1024 0)))

;(pm-reload)

;; Helpers
(defun any? (i l)
   (if l
       (if (eq i (car l))
 	  (cons i (any? i (cdr l)))
 	  (any? i (cdr l)))))

;; ROTATE SCALES
;; (defun rotate (scale) (append (cdr scale) (list (car scale))))
;; (defun rotate-n (n scale)
;;   (if (> n 0)
;;       (rotate-n (- n 1) (rotate scale))
;;       scale))

(defun map-idx (s)
  (let ((idx 0))
    (mapcar
     (lambda (i)
       (let ((r (cons i idx)))
	 (setf idx (+ 1 idx))
	 r)  )
     s)))

(defun find-solfege1 (los lis)
  (if (and los lis)
      (if (not (listp (cdr (car lis))))
	  (if (eq (car los) (cdr (car lis)))
	      (cons (car lis) (find-solfege1 (cdr los) (cdr lis)))
	      (find-solfege1 los (cdr lis)))
	  (if (any? (car los) (cdr (car lis)))
	      (cons (car lis) (find-solfege1 (cdr los) (cdr lis)))
	      (find-solfege1 los (cdr lis))))))

;; Scale Functions
(defun make-scale (steps solfege)
  (if steps
      (cons (cons (car steps) (car solfege))
	    (make-scale (cdr steps) (cdr solfege)))))
(defun chromatic-scale ()
  (make-scale '(h h h h h h h h h h h h) '(do (di ra) (re) (ri me) mi fa (fi se) so (si le) la (li te) ti do)))
(defun major-scale () (make-scale '(w w h w w w h) '(do re mi fa so la ti do)))
(defun minor-scale () (make-scale '(w h w w h w w) '(do re me fa so le te do)))
(defun dorian-scale () (make-scale '(w h w w w h) '(do re me fa so la ti do)))
(defun phrygian-scale () (make-scale '(h w w w h w) '(do ra me fa so le te do)))

(defun scaleints (scale)
  (mapcar (lambda (i) (if (eq 'w (car i))
			  (cons 2 (cdr i))
			  (cons 1 (cdr i)))) scale))

(defun midi-note-octave ()
  '(A0 A#0 B0 C0 C#0 D0 D#0 E0    F0    F#0    G0    G#0 A1    A#1    B1    C1    C#1    D1    D#1    E1    F1    F#1    G1    G#1 A2    A#2    B2    C2    C#2    D2    D#2    E2    F2    F#2    G2    G#2 A3    A#3    B3    C3    C#3    D3    D#3    E3    F3    F#3    G3    G#3  A4    A#4    B4    C4    C#4    D4    D#4    E4    F4    F#4    G4    G#4 A5    A#5    B5    C5    C#5    D5    D#5    E5    F5    F#5    G5    G#5 A6    A#6    B6    C6    C#6    D6    D#6    E6    F6    F#6    G6    G#6  A7    A#7    B7    C7    C#7    D7    D#7    E7    F7    F#7    G7    G#7 A8    A#8    B8    C8    C#8    D8    D#8    E8    F8    F#8    G8    G#8 A9  A#9    B9    C9    C#9    D9    D#9    E9    F9    F#9    G9))

(defun midi-notes () (loop for x from 0 to 87 collect (cons (nth x (midi-note-octave)) (+ 21 x))))

(defun midi-notes-from-scale (midi-notes original-scale scale)
  (if (and midi-notes scale)
      (if (eq 2 (car (car (scaleints scale))))
	  (cons (cons (car midi-notes) (cdr (car (scaleints scale))))
		(midi-notes-from-scale (cdr (cdr midi-notes)) original-scale (cdr scale)))
	  (cons (cons (car midi-notes) (cdr (car (scaleints scale))))
		(midi-notes-from-scale (cdr midi-notes) original-scale (cdr scale))))
      (if (and (not scale) midi-notes)
	  (midi-notes-from-scale midi-notes original-scale original-scale))))

(defun scale-range (n1 n2 scale)
  (let* ((eqfn (lambda (x y) (eq (note-name (make-note x)) (car y))))
	 (p1 (position n1 scale :test eqfn ))
	(p2 (position n2 scale :test eqfn)))
    (subseq scale p1 (+ 1 p2))))

(defun solfege-scale-template (scale-template) (mapcar #'cdr scale-template))

;;FIXME
;;(defun modes (scale) (mapcar (lambda (i) (rotate-n (cdr i) scale)) (map-idx scale)))

(defun make-scale-from-pattern (p1 p2 scale-pattern)
  (midi-notes-from-scale (scale-range p1 p2 (midi-notes)) scale-pattern scale-pattern))

(defvar *current-scale* nil)
(defun set-scale (scale)
  (setf *current-scale* scale))

;; Note functions
(defun make-note (n &optional (notes (midi-notes)))
  (if notes
      (if (eq n (car (car notes)))
	  (car notes)
	  (make-note n (cdr notes)))))
(defun note-name (note) (car note))
(defun note-value (note) (cdr note))
;(pm-reload)
;(note-play (make-note 'c4))

(defun note-play (note)
  (pm:write-short-midi *midi-out3* 1 (pm:note-on 1 (note-value note) 80))
  (pm:write-short-midi *midi-out3* 1 (pm:note-off 1 (note-value note) 0)))

(defun note-play-sleep (note)
  (pm:write-short-midi *midi-out3* 1 (pm:note-on 1 (note-value note) 80))
  (sleep 0.25)
  (pm:write-short-midi *midi-out3* 1 (pm:note-off 1 (note-value note) 0)))

(defun prev-note (n l)
  (if (and n l)
      (if (equal n
		 (car (cdr l)))
	  (car l)
	  (prev-note n (cdr l)))))
(defun prev-notes (n note)
  (if (> n 0)
      (prev-notes (- n 1) (prev-note note (midi-notes)))
      note))
(defun next-note (n l)
  (if (and n l)
      (if (equal n
	      (car (cdr l)))
	  (car (cdr l))
	  (next-note n (cdr l)))))
(defun next-notes (n note)
  (if (> n 0)
      (next-notes (- n 1) (next-note note (midi-notes)))
      note))

(defun octave-down (note) (prev-notes 12 note))
(defun octave-up (note) (next-notes 12 note))

;;MIDI functions

(defun play-scale (scale)
  (dolist (n scale)
    (note-play (car n))
    (pm:write-short-midi *midi-out3* 1 (pm:note-on 1 (note-value (car n)) 80))
    (pm:write-short-midi *midi-out3* 1 (pm:note-off 1 (note-value (car n)) 0))
    (sleep 0.1)))

(defun solfege-chord (l) (dolist (x (find-solfege1 l *current-scale*)) (note-play (car x))))

(defun arp (l) (dolist
		   (x (find-solfege1 l *current-scale*))
		 (note-play (car x)) (sleep 0.5)))
(defun darp (l) (find-solfege1 l *current-scale*))
(defun rarp (l)
  (let ((m (reverse (find-solfege1 l *current-scale*))))
    (dolist
	(x m)
      (note-play (car x)) (sleep 0.5))))

(defmacro c (fn &body body) `(,fn (list ,@(mapcar (lambda (x) `',x) body))))

(defun play-tonic (scale) (note-play (car (car scale))))
(defun play-subdominant (scale) (note-play (car (nth 3 scale))))
(defun play-dominant (scale) (note-play (car (nth 4 scale))))

(defun play-tonic-subdominant-dominant (scale)
  (progn
    (note-play (octave-down (car (car scale))))
    (chord '(DO MI SO))
    (sleep 0.5)
    (chord '(FA LA DO))
    (sleep 0.5)
    (chord '(SO TI RE))
    (sleep 0.5)
    (play-tonic scale)))

(defun random-note (scale) (nth (random (length scale)) scale))
(defun random-notes (y) (loop for x from 1 to y collect (random-note *current-scale*)))
(defun play-random (scale) (note-play (car (random-note scale))))
(defun note-solfege (note) (cdr note))

(defun set-random-scale ()
  (let* ((letters '(A B C D E F G))
	 (random-letter (nth (random (length letters)) letters)))
    (set-scale (make-scale-from-pattern (intern (format nil "~A~d" random-letter 3))
					(intern (format nil "~A~d" random-letter 4))
					(major-scale)))))
