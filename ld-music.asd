(defsystem #:ld-music
  :description "Playing with music stuff"
  :author "Lake Denman"
  :serial t
  :depends-on (#:portmidi #:cl-ppcre #:midi #:yason)
  :components ((:module "src"
		:serial t
		:components
		((:file "package")
		 (:file "random")
		 (:file "output")
		 (:file "util")
		 (:file "midi")
		 (:file "chord")
		 (:file "note")
		 (:file "scale")
		 (:file "play")
		 (:file "games")
		 ))))
