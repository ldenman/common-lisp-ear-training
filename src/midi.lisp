(in-package :ld-music)

;; TODO - don't use globals
(defvar *midi-out3* nil)
(setf *midi-out3* nil)

(defun my-midi-setup ()
  (launch-qsynth)
  (sleep 2)
  (pm:initialize)
  (setup-midi))

(defun midi-unload ()
  (pm-terminate)
  (kill-qsynth))

(defun launch-qsynth ()
  (uiop:launch-program "qsynth"))
(defun kill-qsynth ()
  (uiop:run-program "killall -9 qsynth" :ignore-error-status t))

(defun setup-midi ()
  (princ "CHOOSE YOUR MIDI DEVICE")
  (pm:initialize)
  (princ (pm:list-devices))
  (let ((id (read-line)))
    (pm-reload (parse-integer id)))
  (smoke-test))

;; TODO - don't use globals
(defun pm-reload (midi-device-id)
  (pm:terminate)
  (pm:initialize)
  (setf *midi-out3* (pm:open-output midi-device-id 1024 0)))
;(pm:close-midi *midi-out3*)
;(pm:list-devices)
;(setup-midi)
;(pm-reload 2)
;(smoke-test)
(defun ensure-midi ()
  (if *midi-out3*
      t
      (error "MIDI not set up. Use (setup-midi)" )))

;; TODO - don't use globals
(defun pm-terminate ()
  (if *midi-out3*
      (progn
	(let ((oldid *midi-out3*))
	  (pm:close-midi oldid)
	  (setf *midi-out3* nil))))
  (pm:list-devices)
  (pm:terminate))

(defun pm-initialize (midi-device-id)
  (pm-reload midi-device-id))

(defun midi-instruments () '(
			     (1 . "Acoustic Grand Piano")(2 . "Bright Acoustic Piano")(3 . "Electric Grand Piano")(4 . "Honky-tonk Piano")(5 . "Electric Piano 1")(6 . "Electric Piano 2")(7 . "Harpsichord")(8 . "Clavinet")(9 . "Celesta")(10 . "Glockenspiel")(11 . "Music Box")(12 . "Vibraphone")(13 . "Marimba")(14 . "Xylophone")(15 . "Tubular Bells")(16 . "Dulcimer")(17 . "Drawbar Organ")(18 . "Percussive Organ")(19 . "Rock Organ")(20 . "Church Organ")(21 . "Reed Organ")(22 . "Accordion")(23 . "Harmonica")(24 . "Tango Accordion")(25 . "Acoustic Guitar (nylon)")(26 . "Acoustic Guitar (steel)")(27 . "Electric Guitar (jazz)")(28 . "Electric Guitar (clean)")(29 . "Electric Guitar (muted)")(30 . "Overdriven Guitar")(31 . "Distortion Guitar")(32 . "Guitar harmonics")(33 . "Acoustic Bass")(34 . "Electric Bass (finger)")(35 . "Electric Bass (pick)")(36 . "Fretless Bass")(37 . "Slap Bass 1")(38 . "Slap Bass 2")(39 . "Synth Bass 1")(40 . "Synth Bass 2")(41 . "Violin")(42 . "Viola")(43 . "Cello")(44 . "Contrabass")(45 . "Tremolo Strings")(46 . "Pizzicato Strings")(47 . "Orchestral Harp")(48 . "Timpani")(49 . "String Ensemble 1")(50 . "String Ensemble 2")(51 . "Synth Strings 1")(52 . "Synth Strings 2")(53 . "Choir Aahs")(54 . "Voice Oohs")(55 . "Synth Voice")(56 . "Orchestra Hit")(57 . "Trumpet")(58 . "Trombone")(59 . "Tuba")(60 . "Muted Trumpet")(61 . "French Horn")(62 . "Brass Section")(63 . "Synth Brass 1")(64 . "Synth Brass 2")(65 . "Soprano Sax")(66 . "Alto Sax")(67 . "Tenor Sax")(68 . "Baritone Sax")(69 . "Oboe")(70 . "English Horn")(71 . "Bassoon")(72 . "Clarinet")(73 . "Piccolo")(74 . "Flute")(75 . "Recorder")(76 . "Pan Flute")(77 . "Blown Bottle")(78 . "Shakuhachi")(79 . "Whistle")(80 . "Ocarina")(81 . "Lead 1 (square)")(82 . "Lead 2 (sawtooth)")(83 . "Lead 3 (calliope)")(84 . "Lead 4 (chiff)")(85 . "Lead 5 (charang)")(86 . "Lead 6 (voice)")(87 . "Lead 7 (fifths)")(88 . "Lead 8 (bass + lead)")(89 . "Pad 1 (new age)")(90 . "Pad 2 (warm)")(91 . "Pad 3 (polysynth)")(92 . "Pad 4 (choir)")(93 . "Pad 5 (bowed)")(94 . "Pad 6 (metallic)")(95 . "Pad 7 (halo)")(96 . "Pad 8 (sweep)")(97 . "FX 1 (rain)")(98 . "FX 2 (soundtrack)")(99 . "FX 3 (crystal)")(100 . "FX 4 (atmosphere)")(101 . "FX 5 (brightness)")(102 . "FX 6 (goblins)")(103 . "FX 7 (echoes)")(104 . "FX 8 (sci-fi)")(105 . "Sitar")(106 . "Banjo")(107 . "Shamisen")(108 . "Koto")(109 . "Kalimba")(110 . "Bag pipe")(111 . "Fiddle")(112 . "Shanai")(113 . "Tinkle Bell")(114 . "Agogo")(115 . "Steel Drums")(116 . "Woodblock")(117 . "Taiko Drum")(118 . "Melodic Tom")(119 . "Synth Drum")(120 . "Reverse Cymbal")(121 . "Guitar Fret Noise")(122 . "Breath Noise")(123 . "Seashore")(124 . "Bird Tweet")(125 . "Telephone Ring")(126 . "Helicopter")(127 . "Applause")			     (  128 . "Gunshot")))

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

(defun make-message (status data1 data2)
  "=> an integer representing a MIDI message
Combines the integers `status`, `data1` and `data2` to a MIDI message."
  (let ((d2 (boole boole-and (ash data2 16) #xFF0000))
	(d1 (boole boole-and (ash data1 8) #xFF00))
	(st (boole boole-and status #xFF)))
    (boole boole-ior d2
	   (boole boole-ior d1 st))))

(defun make-message* (upper lower data1 data2) ;internal
  "=> a MIDI message as an integer
Works like `make-message` but combines `upper` and `lower` to the status byte."
  (let ((status (boole boole-ior
		       (boole boole-and (ash upper 4) #xF0)
		       (boole boole-and lower #xF))))
    (make-message status data1 data2)))

(defun program-change (program &optional (channel 1) (stream *midi-out3*))
  (pm:write-short-midi stream 0 (make-message* #0xC channel program  0))
  program)

(defun panic (&optional (channel 1))
  "Stop all notes on a channel of a midi stream."
  (loop for x in (midi-notes) do (note-stop x)))

(defun note-on (value &optional (velocity 80) (channel 0) (stream *midi-out3*))
  "Play a midi note."
  (pm:write-short-midi stream 0 (pm:note-on channel value velocity)))
(defun note-off (value &optional (channel 0) (stream *midi-out3*))
  "Stop a midi note."
  (pm:write-short-midi stream 0 (pm:note-off channel value 0)))

(defun notes-on (values &optional (velocity 80) (channel 0) (stream *midi-out3*))
  "Play multiple midi notes."
  (loop :for value :in values :do (note-on value velocity channel stream)))
(defun notes-off (values &optional (channel 0) (stream *midi-out3*))
  "Stop multiple midi notes."
  (loop :for value :in values :do (note-off value channel stream)))

(defun note-play (note &optional (velocity 80) (channel 0))
  (pm:write-short-midi *midi-out3* 0 (pm:note-on channel (note-value note) velocity)))
(defun note-stop (note &optional (channel 0))
  (princ (note-value note))
  (pm:write-short-midi *midi-out3* 0 (pm:note-off channel (note-value note) 0)))

(defun note-play-sleep (note)
  (pm:write-short-midi *midi-out3* 1 (pm:note-on 1 (note-value note) 80))
  (sleep 0.25)
  (pm:write-short-midi *midi-out3* 1 (pm:note-off 1 (note-value note) 0)))

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
