(in-package :o2/examples)

(defun main ()
  ;; You can use default application class without subclassing.
  ;; Application will create default renderer automatically and all you need is:
  ;; * Register any state (instance of base class is suitable too).
  ;; * Add some entities to it with desired components.
  ;; * Call `application:start' method. This will start the application main
  ;;   loop.
  (let* ((app (make-instance 'o2/engine:application))
         (state (o2/engine:register-state app 'o2/engine:state))
         (obj (o2/engine:make-game-object
               :components ((o2/engine:transform-c :position (cons -200 -300))
                            (o2/engine:render-c :sprite :sprite-not-found
                                                :render-priority 1)))))
    (log:info "Created " app "with" state "and object" obj)
    (o2/engine:add-object state obj)
    (o2/engine:start app)))

