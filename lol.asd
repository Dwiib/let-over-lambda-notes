(defsystem "lol"
  :depends-on ("alexandria" "trivia" "iterate" "fare-quasiquote-extras")
  :components ((:file "package") (:file "ch2" :depends-on ("package"))
               (:file "ch3" :depends-on ("package"))))
