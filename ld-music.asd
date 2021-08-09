(defsystem #:ld-music
  :description "Playing with music stuff"
  :author "Lake Denman"
  :serial t
  :depends-on (#:portmidi #:cl-ppcre #:midi)
  :components ((:module "src"
		:serial t
		:components
		((:file "package")
		 (:file "util")
		 (:file "midi")
		 (:file "chord")
		 (:file "note")
		 (:file "scale")
		 (:file "play")
		 (:file "games")
		 ))))
