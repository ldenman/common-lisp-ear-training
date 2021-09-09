(in-package :ld-music)

(defvar *midi-channel*)

(defun schedule (time fn &rest args)
  "Schedule a function to be called after some amount of seconds."
  (schedule-timer
   (make-timer
    (lambda ()
      (apply fn args))
    :thread t)
   time))

(defun schedule-note (note &optional (on-time 0) off-time (velocity 80))
  "Play a note."
  (let ((value (note-value note))
	(off-time (or off-time (+ on-time 1))))
    (schedule on-time #'note-play note velocity)
    (schedule off-time #'note-off value)))

;; Make on and off message for note
;; internal
(defun note->midi-message (note time-on time-off &optional (*midi-channel* 0))
  (list
   (make-instance 'midi:note-on-message
		  :time time-on
		  :key (note-value note)
		  :velocity 100
		  :status (+ 144 *midi-channel*))
   (make-instance 'midi:note-off-message
		  :time time-off
		  :key (note-value note)
		  :velocity 100
		  :status (+ 128 *midi-channel*))))

;; Make MIDI timing track for TEMPO and TIME SIGNATURE
;; TODO - Open ticket with cl-portmidi about 'midi:dd/nn/cc/bb exports
(defun midi-timing-track (bpm &optional (*midi-channel* 9))
  (let ((timing-track 
	  (list (make-instance 'midi:time-signature-message
			       :status 255
			       :time 0)
		(make-instance 'midi:tempo-message
			       :time 0
			       :tempo (round 60000000 bpm)))))
    (setf (slot-value (first timing-track) 'midi:dd) 2
          (slot-value (first timing-track) 'midi:nn) 4
          (slot-value (first timing-track) 'midi:cc) 24
          (slot-value (first timing-track) 'midi:bb) 8)
    timing-track))

;; make list of midi tracks for MIDIFILE output
(defun midi-seq-format-1 (rhythmic-notes &optional (bpm 60))
  (list (midi-timing-track bpm)
	(rhythmic-notes->midi-messages rhythmic-notes bpm)))

;; make list of MIDI messages for MIDIFILE output format 0
;; no rhythm
(defun midi-seq-format-0 (notes)
  (let* ((duration 60)
	 (time 0)
	 (result '()))
    (dolist (note notes)
      (setf result
	    (append
	     result
	     (list
	      (make-instance 'midi:note-on-message
			     :time time
			     :key (note-value note)
			     :velocity 100
			     :status 144)
	      (make-instance 'midi:note-off-message
			     :time (+ time duration)
			     :key (note-value note)
			     :velocity 100
			     :status 128)
	      )))
      (incf time duration))
    result))

