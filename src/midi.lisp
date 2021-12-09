(in-package :ld-music)

;; holds references to MIDI OUT interface
(defvar *midi-out* nil)

;; This is the MIDI interface implemented as a closure.  This handles
;; the setup/teardown of the MIDI device.
;; Exposes functions:
;; #'midi-stream, #'midi-stream=, #'state, #'smoke-test, #'reset,
;; #'setup, #'close-connections, #'close
(defun midi ()
  (let ((midi-stream nil)
	(self nil))
    (lambda (operation &rest arguments)
      (ecase operation
	(self self)
	(initialize (unless self (setq self (car arguments))))
	(midi-stream midi-stream)
	(midi-stream=  (setq midi-stream (car arguments)))
	(state (if midi-stream 'open 'closed))
	(note-on (apply #'note-on arguments))
	(smoke-test (if midi-stream
			(progn
			  (note-on 72)
			  (sleep 1)
			  (note-off 72))
			(error "Device is closed")))
	(reset (progn (funcall self 'close) (funcall self 'setup)))
	(setup (if midi-stream
		   (error "Device already open")
		   (let ((id (or (car arguments) -1)))
                     ;;reinit portmidi for hotloading midi devices
		     (pm:terminate)
		     (pm:initialize)

		     (if (and id (> id -1))
			 (progn
			   (funcall self 'midi-stream= (pm:open-output id 1024 0)))
			 (progn
			   (princ "CHOOSE YOUR MIDI DEVICE")
			   (princ (pm:list-devices))
			   (let ((id (read-line)))
			     (funcall self 'midi-stream= (pm:open-output (parse-integer id) 1024 0))
			     (funcall self 'smoke-test)))))))

	(close
	 (if midi-stream
	     (progn
	       (pm:abort-midi midi-stream)
	       (setq midi-stream (pm:close-midi midi-stream)))
	     (error "Device already closed")))))))

(defun init-midi! ()
  (or *midi-out*
      (let ((midi-interface (midi)))
	(funcall midi-interface 'initialize midi-interface)
	(setf *midi-out* midi-interface))))

(defun midi-reset! ()
  (ignore-errors (midi-close))
  (setf *midi-out* nil)
  (init-midi!))

(defun midi-state (&optional (dev *midi-out*))
  (funcall dev 'state))

(defun midi-stream (&optional (dev *midi-out*))
  (funcall dev 'midi-stream))
(defun midi-setup (&optional (id -1) (dev *midi-out*))
  (funcall dev 'setup id))
(defun midi-close (&optional (dev *midi-out*))
  (funcall dev 'close))
(defun midi-reset  (&optional (dev *midi-out*))
  (funcall dev 'reset))
(defun smoke-test (&optional (dev *midi-out*))
  (funcall dev 'smoke-test))

(defun launch-qsynth ()
  (uiop:launch-program "qsynth"))
(defun kill-qsynth ()
  (uiop:run-program "killall -9 qsynth" :ignore-error-status t))

(defun ensure-midi ()
  (if (midi-stream)
      t
      (error "MIDI not set up. Use (setup-midi)" )))

;; internal
(defun midi-note-octave ()
  '(A0 A#0 B0 C0 C#0 D0 D#0 E0    F0    F#0    G0    G#0 A1    A#1    B1    C1    C#1    D1    D#1    E1    F1    F#1    G1    G#1 A2    A#2    B2    C2    C#2    D2    D#2    E2    F2    F#2    G2    G#2 A3    A#3    B3    C3    C#3    D3    D#3    E3    F3    F#3    G3    G#3  A4    A#4    B4    C4    C#4    D4    D#4    E4    F4    F#4    G4    G#4 A5    A#5    B5    C5    C#5    D5    D#5    E5    F5    F#5    G5    G#5 A6    A#6    B6    C6    C#6    D6    D#6    E6    F6    F#6    G6    G#6  A7    A#7    B7    C7))
;;internal
(defun midi-integers () (loop for x from 0 to 87 collect (+ 21 x)))

;; external
(defun midi-notes ()
  (loop for octave in (midi-note-octave)
	for integer in  (midi-integers)
	collect (make-note octave integer nil)))

(defun program-change (program &optional (channel 0) (stream (midi-stream)))
  (pm:write-short-midi stream 0 (pm:make-message* 12 channel program  0))
  program)

(defun panic (&optional (channel 0))
  "Stop all notes on a channel of a midi stream."
  (loop for x in (midi-notes) do (note-stop x channel)))

(defun setup ()
  (launch-qsynth)
  (sleep 3)
  (midi-setup 10))

(defun note-on (value &optional (velocity 80) (channel 0) (stream (midi-stream)))
  "Play a midi note."
  (pm:write-short-midi stream 0 (pm:note-on channel value velocity)))
(defun note-off (value &optional (channel 0) (stream (midi-stream)))
  "Stop a midi note."
  (pm:write-short-midi stream 0 (pm:note-off channel value 127)))

(defun notes-on (values &optional (velocity 80) (channel 0) (stream (midi-stream)))
  "Play multiple midi notes."
  (loop :for value :in values :do (note-on value velocity channel stream)))
(defun notes-off (values &optional (channel 0) (stream (midi-stream)))
  "Stop multiple midi notes."
  (loop :for value :in values :do (note-off value channel stream)))


(defun note-play (note &optional (velocity 80) (channel 0))
  (funcall *midi-out* 'note-on (note-value note) velocity channel))

;; (defun note-play2 (note &optional (velocity 80) (channel 0))
;;   (pm:write-short-midi (midi-stream) 0 (pm:note-on channel (note-value note) 80)))

(defun note-stop (note &optional (channel 0))
  (princ (note-value note))
  (pm:write-short-midi (midi-stream) 0 (pm:note-off channel (note-value note) 0)))

(defun note-play-sleep (note)
  (pm:write-short-midi (midi-stream) 1 (pm:note-on 1 (note-value note) 80))
  (sleep 0.25)
  (pm:write-short-midi (midi-stream) 1 (pm:note-off 1 (note-value note) 0)))

(defun write-midi-file-format-0 (outfile midi-notes)
  (let* ((my-midi-file (make-instance 'midi:midifile
				      :format 0
				      :tracks (list midi-notes)
				      :division 60)))
    (midi:write-midi-file my-midi-file outfile)))

(defun write-midi-file-format-1 (outfile midi-notes &optional (bpm 60))
  (let* ((my-midi-file (make-instance 'midi:midifile
				      :format 1
				      :tracks (midi-seq-format-1 midi-notes bpm)
				      :division bpm)))
    (midi:write-midi-file my-midi-file outfile)))



;;;; Initialize the *midi-out* variable
(init-midi!)
(setup)
;; (midi-setup)
;; (pm:terminate)
;; (pm:initialize)
;; (midi-reset!)
;; (midi-setup)
