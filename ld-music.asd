(defsystem "ld-music"
  :description "Playing with music stuff"
  :author "Lake Denman"
  :serial t
  :depends-on ("portmidi" "cl-ppcre" "midi" "yason" "arrow-macros")
  :components ((:module "src"
		:serial t
		:components
		((:file "package")
		 (:file "types")
		 (:file "random")
		 (:file "util")
		 (:file "midi")
		 (:file "chord")
		 (:file "note")
		 (:file "scale")
		 (:file "solfege")
		 (:file "play")
		 (:file "games")
                 (:file "sequencer")
                 (:file "rhythm")
		 (:file "event")
		 (:file "dsl")
		 (:file "et/progressions")))))

(defsystem "ld-music/tests"
  :description "Playing with music stuff"
  :author "Lake Denman"
  :pathname "src/t/"
  :depends-on ("ld-music")
  :in-order-to ((asdf:test-op (asdf:load-op :ld-music/tests)))
  :components 	((:file "test")
		 (:file "tests" :depends-on ("test"))
		 (:file "tdsl" :depends-on ("test"))
		 (:file "play-test" :depends-on ("test")))
  :perform (test-op (o c)
		    (if (not (symbol-call :ld-music '#:run-tests))
			(error "testing failed"))))
