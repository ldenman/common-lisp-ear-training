(in-package :ld-music)

(defun write-midi-file2 (outfile midi-notes &optional (division 60))
  (let* ((my-midi-file (make-instance 'midi:midifile
				      :format 1
				      :tracks midi-notes
				      :division division)))
    (midi:write-midi-file my-midi-file outfile)))

;(write-midi-file2 "output2.midi" (little-sequence))
;(write-midi-file2 "output1.midi" (little-sequence 90) 90)

(defun make-midi-seq2 (notes &optional (bpm 60))
  (let* ((time 0)
	 (result '())
	 (*midi-channel* 9)
	 (timing-track
	   (list (make-instance 'midi:time-signature-message
				:status #0xFF
				:time 0)
		 (make-instance 'midi:tempo-message
				:time 0
				:tempo (round 60000000 bpm)))))
    (let ((*midi-channel* 0))

      (dolist (item notes)
	(let ((rhythm (cdr item))
	      (note (car item)))
	  (setf result
		(append
		 result
		 (list
		  (make-instance 'midi:note-on-message
				 :time time
				 :key (note-value note)
				 :velocity 100
				 :status (+ 144 *midi-channel*))
		  (make-instance 'midi:note-off-message
				 :time (+ time (rhtyhm->duration rhythm bpm))
				 :key (note-value note)
				 :velocity 100
				 :status (+ 128 *midi-channel*))
		  )))
	  (incf time (rhtyhm->duration rhythm bpm)))))

    (setf (slot-value (first timing-track) 'midi:dd) 2
          (slot-value (first timing-track) 'midi:nn) 4
          (slot-value (first timing-track) 'midi:cc) 24
          (slot-value (first timing-track) 'midi:bb) 8)

    (list
     timing-track
     result)
    ))

(defun add-note-rhythms (notes rhythm-list)
  (pairup notes rhythm-list))

(defun little-sequence (&optional (division 60))
  (make-midi-seq2 (add-note-rhythms
		   (take 15 (attr 'notes (scale-range2 'c3 'c5 (make-scale 'c4))))		   
 		   (grow (take 15 (attr 'notes (scale-range2 'c3 'c5 (make-scale 'c4))))
			 '(1 2 2 4 4 4 4 8 8 8 8 8 8 8 ))) division))

;; convert rhythm to duration based on bpm

(defun rhtyhm->duration (r bpm)
  (let ((res (case r
	       (1 (*  4 bpm))
	       (2 (*  2 bpm))
	       (4 (*  1 bpm ))
	       (8 (*  0.5 bpm))
	       (16 (* 0.25 bpm ))))
	)
    (round  res)))
