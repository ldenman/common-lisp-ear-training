(in-package :ld-music)

(defun write-midi-file2 (outfile midi-notes &optional (division 60))
  (let* ((my-midi-file (make-instance 'midi:midifile
				      :format 1
				      :tracks midi-notes
				      :division division)))
    (midi:write-midi-file my-midi-file outfile)))

;(write-midi-file2 "output2.midi" (little-sequence))
;(write-midi-file2 "output1.midi" (little-sequence 90) 90)

(defun notes->midi-messages (notes/rhythms bpm &optional (*midi-channel* 0))
  (let ((result '())
	(time 0))
   (dolist (item notes/rhythms)
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
				 :time (+ time (rhythm->duration rhythm bpm))
				 :key (note-value note)
				 :velocity 100
				 :status (+ 128 *midi-channel*))
		  )))
	  (incf time (rhythm->duration rhythm bpm))))
    result))

(defun make-midi-seq2 (notes &optional (bpm 60))
  (let* ((*midi-channel* 9)
	 (timing-track
	   (list (make-instance 'midi:time-signature-message
				:status #0xFF
				:time 0)
		 (make-instance 'midi:tempo-message
				:time 0
				:tempo (round 60000000 bpm)))))


    (setf (slot-value (first timing-track) 'midi:dd) 2
          (slot-value (first timing-track) 'midi:nn) 4
          (slot-value (first timing-track) 'midi:cc) 24
          (slot-value (first timing-track) 'midi:bb) 8)

    (list
     timing-track
    (let ((*midi-channel* 0))
      (notes->midi-messages notes)))))

(defun add-note-rhythms (notes rhythm-list)
  (pairup notes rhythm-list))

(defun little-sequence (&optional (division 60))
  (make-midi-seq2 (add-note-rhythms
		   (take 15 (attr 'notes (scale-range2 'c3 'c5 (make-scale 'c4))))		   
 		   (grow (take 15 (attr 'notes (scale-range2 'c3 'c5 (make-scale 'c4))))
			 '(1 2 2 4 4 4 4 8 8 8 8 8 8 8 ))) division))

(defun make-motif (scale &rest solfege)
  (mapcar (lambda (s) (find-solfege s (attr 'notes scale))) solfege))
(defun make-rhythmic (notes rhythm-list)
  (add-note-rhythms notes rhythm-list))


;; convert rhythm to duration based on bpm

(defun rhythm->duration (r bpm)
  (let ((res (case r
	       (1 (*  4 bpm))
	       (2 (*  2 bpm))
	       (4 (*  1 bpm ))
	       (8 (*  0.5 bpm))
	       (16 (* 0.25 bpm ))))
	)
    (round  res)))

(defun schedule (time fn &rest args)
  "Schedule a function to be called after some amount of seconds."
  (schedule-timer
   (make-timer
    (lambda ()
      (apply fn args))
    :thread t)
   time))

(defun note-on (value &optional (velocity 80) (channel 0) (stream *midi-out3*))
  "Play a midi note."
  (pm:write-short-midi stream 0 (pm:note-on channel value velocity)))
(defun note-off (value &optional (channel 0) (stream *midi-out3*))
  "Stop a midi note."
  (pm:write-short-midi stream 0 (pm:note-off channel value 0)))

(defun play-note2 (note &optional (on-time 0) off-time (velocity 80))
  "Play a note."
  (let ((value (note-value note))
	(off-time (or off-time (+ on-time 1))))
    (schedule on-time #'note-on value velocity)
    (schedule off-time #'note-off value)
    ))

;; (play2
;;  (make-event (car (attr 'notes (scale-range2 'c3 'c5 (make-scale 'c3)))) 0 1 80))

(defun play2 (event)
  "Play an event."
  (play-note2 (attr 'note event)
	      (attr 'on-time event)
	      (attr 'off-time event)
	      (attr 'velocity event)))

(defun notes-on (values &optional (velocity 80) (channel 0) (stream *midi-out3*))
  "Play multiple midi notes."
  (loop :for value :in values :do (note-on value velocity channel stream)))

(defun notes-off (values &optional (channel 0) (stream *midi-out3*))
  "Stop multiple midi notes."
  (loop :for value :in values :do (note-off value channel stream)))

(defun play-notes2 (events &optional (on-time 0) off-time (velocity 80))
  "Play multiple notes."
  (let ((values (mapcar (lambda (event) (note-value (attr 'note event))) events))
	(off-time (or off-time (+ on-time 1))))
    (schedule on-time #'notes-on values velocity)
    (schedule off-time #'notes-off values)))

(defun play-notes3 (events)
  "Play multiple notes."
  (dolist (event events)
    (play2 event)
    (sleep 0.001)))

(defun make-event (note on-time off-time velocity)
  (list
   (cons 'note note)
   (cons 'on-time on-time)
   (cons 'off-time off-time)
   (cons 'velocity velocity)))

;; (pm-reload 2)
;; (smoke-test)

;; (play-notes3
;;   (notes->pm-event
;;    (make-rhythmic
;;     (make-motif (scale-range2 'c3 'c5 (make-scale 'c3)) 'do 'mi 'so) '(4 4 4)) 200))

(defun beat-length (beat bpm)
  (/ 60 (/ bpm beat)))
(defun r->d (r bpm)
    (let ((res (case r
	       (1 4)
	       (2 2)
	       (4 1)
	       (8 0.5)
	       (16 0.25))))
    (beat-length res bpm)))

(defun notes->pm-event (notes/rhythms bpm &optional (*midi-channel* 0))
  (let ((result '())
	(time 0))
    (dolist (item notes/rhythms)
      (let ((rhythm (cdr item))
	    (note (car item)))
	(setf result
	      (append
	       result
	       (list 
		(make-event
		 note
		 time
		 (+ time (r->d rhythm bpm)) 80))))
	(incf time (r->d rhythm bpm))))
    result))



