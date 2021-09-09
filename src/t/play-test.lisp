(in-package :ld-music)

(defun dummy-midi ()
  (let ((calls '()))
    (lambda (operator &rest args)
      (case operator
	(calls calls)
	(t (push (cons operator args) calls))))))

(defvar *fake-midi* 't)
(setf *fake-midi* t)

(defun with-test-midi (fn)
  (let ((*midi-out* (if *fake-midi* (dummy-midi) *midi-out*))
	(*ignore-sleep* (if *fake-midi* 't nil)))
    (funcall fn)
    (if *fake-midi*
	(funcall *midi-out* 'calls))))

(deftest play-note-test ()
  (check
    (equal '((NOTE-ON 21 80 0))
	   (with-test-midi
	       (lambda ()
		 (play (car (_notes (make-scale 'c4)))))))))

(deftest play-notes-test ()
  (check
    (equal '((NOTE-ON 36 80 0) (NOTE-ON 35 80 0) (NOTE-ON 33 80 0) (NOTE-ON 31 80 0) (NOTE-ON 29 80 0) (NOTE-ON 28 80 0) (NOTE-ON 26 80 0) (NOTE-ON 24 80 0) (NOTE-ON 23 80 0) (NOTE-ON 21 80 0))
	   (with-test-midi
	       (lambda ()
		 (play (take 10 (_notes (make-scale 'c4)))))))))

(deftest play-chord-sequence ()
  (check
    (equal '((NOTE-ON 81 80 0) (NOTE-ON 77 80 0) (NOTE-ON 74 80 0) (NOTE-ON 71 80 0)
 (NOTE-ON 67 80 0) (NOTE-ON 64 80 0) (NOTE-ON 60 80 0) (NOTE-ON 88 80 0)
 (NOTE-ON 84 80 0) (NOTE-ON 81 80 0) (NOTE-ON 77 80 0) (NOTE-ON 74 80 0)
 (NOTE-ON 71 80 0) (NOTE-ON 67 80 0) (NOTE-ON 86 80 0) (NOTE-ON 83 80 0)
 (NOTE-ON 79 80 0) (NOTE-ON 76 80 0) (NOTE-ON 72 80 0) (NOTE-ON 69 80 0)
 (NOTE-ON 65 80 0) (NOTE-ON 81 80 0) (NOTE-ON 77 80 0) (NOTE-ON 74 80 0)
 (NOTE-ON 71 80 0) (NOTE-ON 67 80 0) (NOTE-ON 64 80 0) (NOTE-ON 60 80 0))
	   (with-test-midi
	     (lambda ()
	       (play (make-chord-sequence '(I IV V I) (make-scale 'c4))))))))

(deftest play-chord ()
  (check
    (equal '((NOTE-ON 41 80 0) (NOTE-ON 38 80 0) (NOTE-ON 35 80 0) (NOTE-ON 31 80 0) (NOTE-ON 28 80 0) (NOTE-ON 24 80 0) (NOTE-ON 21 80 0))
	   (with-test-midi
	     (lambda ()
	       (play (car (scale-chords (make-scale-chords (make-scale 'c4))))))))))


(deftest test-cadence-melody-sequence ()
  (let ((sequence (make-random-melody-sequence (make-scale 'c4) 0 4)))
    (check
      (cadseq-cadence sequence)
      (cadseq-contents sequence))))

(deftest test-random-progression-sequence ()
  (let ((sequence (make-random-progression-sequence (make-scale 'c4))))
    (check
      (cadseq-cadence sequence)
      (cadseq-contents sequence))))

(deftest test-random-note-sequence ()
  (let ((sequence     (make-random-note-sequence (make-scale 'c4))))
    (check
      (cadseq-cadence sequence)
      (cadseq-contents sequence))))

(deftest test-random-rhythmic-melody-sequence ()
  (let ((sequence     (make-rhythmic-melody-sequence (make-scale 'c4) 2 2)))
    (check
      (cadseq-cadence sequence)
      (cadseq-contents sequence))))

(deftest test-playing-cadence ()
  (check
    (equal '((NOTE-ON 67 80 0) (NOTE-ON 64 80 0) (NOTE-ON 60 80 0) (NOTE-ON 74 80 0)
	     (NOTE-ON 71 80 0) (NOTE-ON 67 80 0) (NOTE-ON 72 80 0) (NOTE-ON 69 80 0)
	     (NOTE-ON 65 80 0) (NOTE-ON 67 80 0) (NOTE-ON 64 80 0) (NOTE-ON 60 80 0))

	   (with-test-midi
	       (lambda ()
		 (let ((cadence-sequence (make-cadence-sequence (make-scale 'c4) nil)))
		   (play (cadseq-cadence cadence-sequence))))))))
