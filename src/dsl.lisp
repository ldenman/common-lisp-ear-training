(in-package :ld-music)

;; ;; row row row your boat
;; (solfa
;;  do 4 do do 8 re mi 4
;;  mi 8 re mi fa so 2 +
;;  do 8 do do - so so so
;;  mi mi mi do do do so 4
;;  fa 8 mi 4 re 8 do 1)

;; ;; mary had a little lamb - single octave
;; (solfa
;;  mi 8 re do re mi mi mi 4
;;  re 8 re re 4 mi 8 so so 4
;;  mi 8 re do re mi mi mi 4 re 8 re mi
;;  re 8 do 1)

(defmacro solfa (&rest l)
  `(play-events (meldsl (quote ,l))))

;;; MELODIC DSL 
(defun newoct (octave fn)
  (if (equal (car fn) '+)
      (+ (cadr fn) octave)
      (- octave (cadr fn))))

(defun meldsl-handle-octaves (s &optional (octave 4))
  "takes in the DSL and adjusts for octave data data:
   Handles converting atoms: '+ '- into (octave . NEWOCT-INT)
   Handles converting (+ INT) and (- INT) into (octave . NEWOCT-INT)"
  (if s
      (cond
	((and (listp (car s)) (member (caar s) '(+ -)))
	 (let* ((data (car s))
		(newoct (newoct octave data)))
	   (meldsl-handle-octaves (append (list
					   (cons 'octave
						 newoct))
					  (cdr s)) newoct)))
	((member (car s) '(+ -))
	 (let ((newoct (if (equal '+ (car s))
			   (+ 1 octave)
			   (- octave 1))))
	   (meldsl-handle-octaves (append (list (cons 'octave newoct)) (cdr s)) newoct)))
	(t (cons (car s) (meldsl-handle-octaves (cdr s) octave))))))

(defun meldsl-handle-solfege (l &optional (scale (make-scale 'c4)) (octave 4))
  "takes in a DSL list and converts solfege to NOTES"
  (if l
      (let ((item (car l)))
	(cond ((listp item)
	       (meldsl-handle-solfege (rest l) scale (rest item)))
	      ((numberp item)
	       (cons item (meldsl-handle-solfege (cdr l) scale octave)))
	      (t (cons (find-solfege2 item (attr 'notes scale) octave)
		       (meldsl-handle-solfege (cdr l) scale octave)))))))

(defun make-repeat (solfege number)
  (list solfege number))

(defun solfege-pairp (i1 i2)
  (and
   (not (numberp i1))
   (numberp i2)))

(defun meldsl-handle-rhythm (l &optional (current-num 0))
  "takes in the shortcut list of notes and rhythms and pairs each note with a rhythm value"
  (if l
      (if (solfege-pairp (first l) (second l))
	  (append (make-repeat (first l) (second l))
		  (meldsl-handle-rhythm (cddr l) (second l)))
	  (append (make-repeat (first l) current-num)
		  (meldsl-handle-rhythm (cdr l) current-num)))))

(defun ->rhythmic-notes (l)
  "convert the DSL list of notes rhythms to rhytmic note pairs"
  (let* ((foo (split-seq #'numberp l))
	 (rhythm (car foo))
	 (melody (cdr foo)))
    (make-rhythmic-notes melody rhythm)))

(defun meldsl (dsl-v1)
  (-> dsl-v1
    (meldsl-handle-octaves)
    (meldsl-handle-solfege)
    (meldsl-handle-rhythm)
    (->rhythmic-notes)
    (rhythmic-notes->pm-events 60)))
