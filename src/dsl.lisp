(in-package :ld-music)
;;; MELODIC SOLFEGE DSL
;;;; SEE DSL EXAMPLES AT BOTTOM OF FILE

;;;; SOLFA MACRO ;;;;
(defmacro solfa (config &rest melody)
  (if (listp config)
      `(play-events (solfadsl (quote ,melody) ,(getf config :bpm)))
      `(play-events (solfadsl (append (list (quote ,config)) (quote ,melody))))))

;;;; OCTAVE HANDLER ;;;;
(defun newoct (octave fn)
  (if (equal (car fn) '+)
      (+ (cadr fn) octave)
      (- octave (cadr fn))))

(defun solfadsl-handle-octaves (s &optional (octave 4))
  "takes in the DSL and adjusts for octave data data:
   Handles converting atoms: '+ '- into (octave . NEWOCT-INT)
   Handles converting (+ INT) and (- INT) into (octave . NEWOCT-INT)"
  (if s
      (cond
	((and (listp (car s)) (member (caar s) '(+ -)))
	 (let* ((data (car s))
		(newoct (newoct octave data)))
	   (solfadsl-handle-octaves (append (list
					   (cons 'octave newoct))
					  (cdr s)) newoct)))
	((member (car s) '(+ -))
	 (let ((newoct (if (equal '+ (car s))
			   (+ 1 octave)
			   (- octave 1))))
	   (solfadsl-handle-octaves (append (list (cons 'octave newoct)) (cdr s)) newoct)))
	(t (cons (car s) (solfadsl-handle-octaves (cdr s) octave))))))
;;;; END OCTAVE HANDLER ;;;;


;;;; SOLFEGE HANDLER ;;;;
(defun solfadsl-handle-solfege (l &optional (scale (make-scale 'c4)) (octave 4))
  "takes in a DSL list and converts solfege to NOTES"
  (if l
      (let ((item (car l)))
	(cond ((listp item)
	       (solfadsl-handle-solfege (rest l) scale (rest item)))
	      ((numberp item)
	       (cons item (solfadsl-handle-solfege (cdr l) scale octave)))
	      (t (cons (find-solfege2 item (scale-notes scale) octave)
		       (solfadsl-handle-solfege (cdr l) scale octave)))))))


;;;; RHYTHM HANDLER ;;;;
(defun solfege-pairp (i1 i2)
  (and
   (not (numberp i1))
   (numberp i2)))

(defun solfadsl-handle-rhythm (l &optional (current-rhythm 4))
  "takes in the shortcut list of notes and rhythms and pairs each note with a rhythm value"
  (if l
      (cond ((solfege-pairp (first l) (second l))
	     (append (list (first l) (second l))
		     (solfadsl-handle-rhythm (cddr l) (second l))))
	    (t (append (list (first l) current-rhythm)
		       (solfadsl-handle-rhythm (cdr l) current-rhythm))))))
;;;; END RHYTHM HANLDER ;;;;



;;;; DSL PIPELINE ;;;;
(defun ->rhythmic-notes (l)
  "convert the DSL list of notes rhythms to rhytmic note pairs"
  (destructuring-bind (rhythm . melody) (split-seq #'numberp l)
    (make-rhythmic-notes melody rhythm)))

(defun solfadsl (dsl-v1 &optional (bpm 60))
  (-> dsl-v1
    (solfadsl-handle-octaves)
    (solfadsl-handle-solfege)
    (solfadsl-handle-rhythm)
    (->rhythmic-notes)
    (rhythmic-notes->pm-events bpm)))
;;;; END DSL PIPELINE ;;;;



;;;; DSL EXAMPLES ;;;;
;;row row row your boat - multiple octaves
(defun rowrowrowyourboat ()
  (solfa
   do 4 do do 8 re mi 4
   mi 8 re mi fa so 2 +
   do 8 do do - so so so
   mi mi mi do do do so 4
   fa 8 mi 4 re 8 do 1))
; (rowrowrowyourboat)

;; mary had a little lamb - single octave
(defun mary-had-a-little-lamb ()
  (solfa
   mi 8 re do re mi mi mi 4
   re 8 re re 4 mi 8 so so 4
   mi 8 re do re mi mi mi 4 re 8 re mi
   re 8 do 1))

; (mary-had-a-little-lamb)
;;;; END DSL EXAMPLES ;;;;
