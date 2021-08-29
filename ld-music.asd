(defsystem #:ld-music
  :description "Playing with music stuff"
  :author "Lake Denman"
  :serial t
  :depends-on (#:portmidi #:cl-ppcre #:midi #:yason #:arrow-macros)
  :components ((:module "src"
		:serial t
		:components
		((:file "package")
		 (:file "random")
		 (:file "util")
		 (:file "midi")
		 (:file "chord")
		 (:file "note")
		 (:file "scale")
		 (:file "play")
		 (:file "games")
                 (:file "sequencer")
                 (:file "rhythm")
		 (:file "event")
		 (:file "dsl")
		 (:file "t/test")
		 (:file "t/tests")))))
