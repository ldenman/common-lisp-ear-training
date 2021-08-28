(in-package :ld-music)

(defun little-sequence (&optional (division 60))
  (midi-seq-format-1
   (make-rhythmic-notes
    (take 15 (scale-notes (scale-range 'c3 'c5 (make-scale 'c4))))   
    (grow (take 15 (scale-notes (scale-range 'c3 'c5 (make-scale 'c4))))
	  '(1 2 2 4 4 4 4 8 8 8 8 8 8 8 ))) division))

;(write-midi-file-format-1 "output2.midi" (little-sequence))
;(write-midi-file-format-1 "output1.midi" (little-sequence 120) 120)

(defun seq (solfege-melody rhythm &optional (scale (scale-range 'c3 'c5 (make-scale 'c3))) (bpm 60))
  (rhythmic-notes->pm-events
   (make-rhythmic-notes
    (solfege->notes scale ( solfege-melody))
    rhythm)
   bpm))


(defun seq2 (melody rhythm &optional (scale (scale-range 'c2 'c5 (make-scale 'c3))) (bpm 60))
  (rhythmic-notes->pm-events
   (make-rhythmic-notes
    melody
    rhythm)
   bpm))

(defun song2 (l &optional (scale (scale-range 'c2 'c5 (make-scale 'c4))) (octave 4))
(if l
	    (cond ((listp (car l))
		   (song2 (cdr l) scale (cdr (car l))))
		  ((numberp (car l))
		   (cons (car l) (song2 (cdr l) scale octave)))
		  (t (cons (find-solfege2 (car l) (attr 'notes scale) octave) (song2 (cdr l) scale octave))))))

(song
 (lengthen-seq
  (song2 (myp '(do 4 do do 8 re mi 4
		mi 8 re mi fa so 2
		+ do 8 do do - so so so
		mi mi mi - do do do
		so 4 fa 8 mi 4 re 8 do 1)))))

(song (lengthen-seq (song2 (myp '(do 4 do do 8 re mi 4
	       mi 8 re mi fa so 2
	       + do 8 do do - so so so
	       mi mi mi - do do do
				  so 4 fa 8 mi 4 re 8 do 1)))))





(defun song (l)
  (let* ((foo (split-seq #'numberp (lengthen-seq l)))
	 (rhythm (car foo))
	 (melody (cdr foo)))
    (seq2 melody rhythm)))

;; (song '(+ do 4 do do re 8 mi 4
;; 	mi 8 re mi fa so 2
;; 	do 8 do do so so so
;; 	mi mi mi do do do
;; 	so 4 fa 8 mi re do 1))

(defun play-seq (&rest args)
  (play-events (apply #'seq args)))
(defun play-seq2 (events)
  (play-events events))

(defun row-row-row-your-boat ()
  (play-seq '(do do do re mi
	      mi re mi fa so
	      do do do so so so 
	      mi mi mi do do do
	      so fa mi re do)
	    '(4 4 8 8 4
	      8 8 8 8 2
	      8 8 8 8 8 8
	      8 8 8 8 8 8
	      4 8 8 8 1)))

; (row-row-row-your-boat)

(defun row-row-row-your-boat2 ()
  (play-seq2 (song '(do 4 do do re 8 mi 4
		     mi 8 re mi fa so 2
		     do 8 do do so so so
		     mi mi mi do do do
		     so 4 fa 8 mi re do 1))))

; (row-row-row-your-boat2)

(defun make-repeat (solfege number)
  (list solfege number))

(defun solfege-pairp (i1 i2)
  (and
   (not (numberp i1))
   (numberp i2)))

(defun lengthen-seq (l &optional (current-num 0))
  (if l
      (if (solfege-pairp (first l) (second l))
	  (append (make-repeat (first l) (second l))
		  (lengthen-seq (cdr (cdr l)) (second l)))
	  (append (make-repeat (first l) current-num)
		  (lengthen-seq (cdr l) current-num)))))

(defun myp (s &optional (octave 4))
  (if s
      (cond ((and (listp (car s))
		  (member (car (car s)) '(+ -)))
	     (let ((newoctave (funcall (car (car s)) octave (or (car (cdr (car s))) 1))))
	       (cons (cons 'octave newoctave) (myp (cdr s) newoctave))))
	    ((member (car s) '(+ -))
	     (myp (push (list (car s)) (cdr s)) (funcall (car s) 1 octave)))
	    (t (cons (car s) (myp (cdr s)))))))

;; (let ((s '((+ 4)
;; 	   do 4
;; 	   re 8)))
;;   (myp s))

;(lengthen-seq '(do 2 re mi fa))



