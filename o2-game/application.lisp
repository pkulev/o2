(in-package :o2-game)

;; TODO: move to resource management -->
(defun res ()
  (merge-pathnames #p"res/" +root+))

(defun res/gfx (file-name)
  (merge-pathnames file-name (merge-pathnames #p"gfx/" (res))))

(defun res/snd (file-name)
  (merge-pathnames file-name (merge-pathnames #p"snd/" (res))))

(defun res/fonts (file-name)
  (merge-pathnames file-name (merge-pathnames #p"fonts/" (res))))
;; TODO: move to resource management <--

(defclass application (o2/engine:application)
  ())

(defun make-application ()
  (make-instance 'application :title "Operation 'Operation'"))

(defmethod o2/engine:start ((app application))

  (o2/engine:init app)
  (let ((current-frame))
    (o2/engine:set-current-renderer
     (make-instance 'o2/engine:renderer
                    :renderer (o2/engine:renderer app)))

    ;; TODO: move to event registration -->
    (log:debug "registering event types...")
    (o2/engine:register-event-type :restart-current-state)
    (o2/engine:register-event-type :change-current-state)
    (log:debug "done")
    ;; TODO: move to event registration <--

    ;; TODO: move to smart asset loading -->
    (log:debug "loading assets...")
    (o2/engine:add-sprite :logo (res/gfx "logo.png"))
    (o2/engine:add-sprite :background (res/gfx "background-placeholder.png"))
    (o2/engine:add-sprite :player (res/gfx "player-pistol-firing.png"))
    (o2/engine:add-sprite :player-sitting (res/gfx "player-sit-pistol-firing.png"))
    (o2/engine:add-sprite :bad-guy (res/gfx "bad-guy-placeholder.png"))
    (o2/engine:add-sprite :G17 (res/gfx "G17.png"))
    (o2/engine:add-sprite :9x19 (res/gfx "9x19.png"))

    (o2/engine:add-font :ubuntu (res/fonts "Ubuntu-R.ttf") :font-size 16)
    (o2/engine:add-font :ubuntu-large (res/fonts "Ubuntu-R.ttf") :font-size 36)
    (log:debug "done")
    ;; TODO: move to smart asset loading <--

    (log:debug "registering states...")
    (o2/engine:register-state app 'main-menu-state)
    (o2/engine:register-state app 'ingame-state)
    (o2/engine:set-state app 'main-menu-state)
    (log:debug "done")

    (with-accessors ((frame-ms o2/engine:frame-ms)
                     (state o2/engine:current-state)) app
      (o2/engine:continuable
        (sdl2:with-event-loop (:method :poll)
          (:restart-current-state ()
                                  ;; FIXME: ugly
                                  (let ((name (class-name (class-of (o2/engine:current-app-state))))
                                        (app (o2/engine:get-current-application)))
                                    (o2/engine:deregister-state app name)
                                    (o2/engine:register-state app name)
                                    (o2/engine:set-state app name)))
          (:change-current-state (:user-data state-name)
                                 (o2/engine:set-state (o2/engine:get-current-application) state-name))
          (:idle ()
                 (log:debug "frame -->")
                 (setf current-frame (sdl2:get-ticks))
                 ;; TODO move out to :before and :after render

                 #+slynk
                 (livesupport:update-repl-link)

                 (sdl2:render-clear (o2/engine:renderer app))

                 ;; FIXME: this is only called to make a physic world step in ingame,
                 ;;        do something proper here
                 (o2/engine:update state)
                 (with-accessors ((objects o2/engine:objects)) state
                   (log:debug "running all systems")
                   (dolist (object objects)
                     (o2/engine:run-systems object)))

                 (sdl2:render-present (o2/engine:renderer app))
                 (let ((current-speed (- (sdl2:get-ticks)
                                         current-frame)))
                   (when (< current-speed frame-ms)
                     (sdl2:delay (round (- frame-ms current-speed)))))
                 (log:debug "frame <--"))
          (:quit ()
                 (log:debug "Got" :quit "event, stopping the application")
                 ;; Clean up states on shutdown
                 (loop for state being the
                       hash-values of (o2/engine:states (o2/engine:get-current-application))
                       do (o2/engine:cleanup state))
                 (when (not (null o2/engine:*physical-component-cleanups*))
                   ;; Run physics components cleanups
                   (dolist (cleanup o2/engine:*physical-component-cleanups*)
                     (funcall cleanup))
                   (setf o2/engine:*physical-component-cleanups* '()))

                 ;; FIXME: this is UGLY, figure out a way to cleanup all the physics and THEN
                 ;; the space. Maybe some kind of cleanup queue for all objects, that runs all
                 ;; cleanups at the end of the step, so that the program arives here,
                 ;; there would be nothing that depends on the physics space
                 (let ((ingame-state (gethash :ingame (o2/engine:states (o2/engine:get-current-application)))))
                   (when ingame-state
                     (with-slots (physical-space) ingame-state
                       (chipmunk:free-space physical-space))))

                 (setf o2/engine:*application* nil)

                 (o2/engine:deinit app)
                 t))))))
