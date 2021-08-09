(in-package :ld-music)
(ql:quickload :cl-json)
(ql:quickload :yason)

(defun make-track () 
  (list
   ;; The STATUS values you give to your messages gives the sequencer channel 
   ;; information but, rather than taking the channel as you'd expect to see it
   ;; (i.e. an integer between 0-15), it takes it in the form the MIDI itself 
   ;; uses, which for NOTE-ON is (+ 144 channel) and for NOTE-OFF is 
   ;; (+ 128 channel).
   (make-instance 'midi:note-on-message
          :time 0
          :key 60 
          :velocity 100
          :status 144)
   (make-instance 'midi:note-off-message
          :time 350
          :key 60 :velocity 100
          :status 128)
   (make-instance 'midi:note-on-message
          :time 350
          :key 62 
          :velocity 100
          :status 144)
   (make-instance 'midi:note-off-message
          :time 700
          :key 62
	  :velocity 100
          :status 128)
   (make-instance 'midi:note-on-message
          :time 370
          :key 64
          :velocity 100
          :status 144)
   (make-instance 'midi:note-off-message
          :time 700
          :key 64
	  :velocity 100
          :status 128)
   (make-instance 'midi:note-on-message
          :time 715
          :key 60
          :velocity 100
          :status 144)
   (make-instance 'midi:note-off-message
          :time 800
          :key 60
	  :velocity 100
          :status 128)))

(defun make-tracks ()
  (list (make-track)))

(defun try-to-write-midi-file (midi-notes)
  (let* ((my-midi-file (make-instance 'midi:midifile
				      :format 0
				      :tracks (list midi-notes)
				      :division 60)))
    (midi:write-midi-file my-midi-file "~/src/remotion-midi-piano-vizualiser/input.mid")))

(defun read-in-frame-file ()
  (with-open-file (stream "~/src/remotion-midi-piano-vizualiser/src/api/midi.json")
    (loop for line = (read-line stream nil)
	  while line
	  collect line)))

(defun parse-midi-to-json ()
  (run-program "/usr/bin/bash" '("--login" "-c" "node src/api/parseMidiToJson.js") :directory "/home/lake/src/remotion-midi-piano-vizualiser/"))

(defun print-entry (key value)
  (format t "~A => ~A~%" key value))

(defun write-solfege-frames (string)
  (with-open-file (stream "/home/lake/src/remotion-midi-piano-vizualiser/src/api/solfege.json"
			  :direction :output)
    (format stream string)))

(defun foo (value notes)
  (let ((x (find-if (lambda (note) (equal value (write-to-string (note-value note)))) notes)))
    (if x
	(symbol-name (note-solfege x)))))

(defun update-json (json notes)
  (let ((newht (make-hash-table))
	(myht (gethash "activeFramePerNote" json)))
    (loop for k being each hash-key of myht using (hash-value v)
	  do (setf (gethash (foo k notes) newht) (append (gethash (foo k notes) newht) v))
    (setf (gethash "activeFramePerNote" json) newht))
    json))

(defun decode-json ()
  (let* ((frame-json (read-in-frame-file))
	 (decoded-json (with-input-from-string (s (car frame-json))
			 (yason:parse s))))
    decoded-json))

(defun test ()
  (with-scale (make-scale 'd4)
    (let* ((notes (scale-range 'd3 'a5 (attr 'notes *current-scale*))))

      (try-to-write-midi-file (n notes))
      (parse-midi-to-json)

      (write-solfege-frames
       (with-output-to-string (*standard-output*)
	 (yason:encode (update-json (decode-json) notes)))))))

(defun n (notes)
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
      (incf time (+ 30 duration)))
    result))

(test)



