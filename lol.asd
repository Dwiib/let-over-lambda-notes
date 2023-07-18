(defsystem "lol"
  :depends-on ("alexandria" "trivia" "iterate")
  :components ((:file "package") (:file "ch1" :depends-on ("package"))))
