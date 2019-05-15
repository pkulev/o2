(in-package :o2)

(defparameter +delay+ (/ 1000.0 30.0) "FPS")

(defvar *application* nil "Instance of the current application.")

(defun res ()
  (merge-pathnames #p"res/" +root+))

(defun res/gfx (file-name)
  (merge-pathnames file-name (merge-pathnames #p"gfx/" (res))))

(defun res/snd (file-name)
  (merge-pathnames file-name (merge-pathnames #p"snd/" (res))))

(defun res/fonts (file-name)
  (merge-pathnames file-name (merge-pathnames #p"fonts/" (res))))

(defclass application ()
  ((states :initform (make-hash-table)
           :reader states)
   (current-state :initform nil
                  :reader current-state)
   (renderer :initform nil)
   (running? :initform nil)))

(defun make-application ()
  (setf *application* (make-instance 'application)))

(defun current-app-state ()
  (current-state *application*))

(defun deregister-state (app state-name)
  (with-slots (states) app
    (when (gethash state-name states)
      (setf (gethash state-name states) nil))))

(defun register-state (app state)
  (with-slots ((ren renderer) states current-state) app
    (if (gethash state states)
        (error "State ~a is already registered." state))

    (let* ((state-object (make-instance state
                                        :application app
                                        :name state
                                        :renderer ren))
           (previous-state current-state))

      (setf (gethash state states) state-object)
      ;; NOTE: Accessing current state on `init'.
      ;; game objects in state's `init' method can access it's state via
      ;; `current-app-state'. To provide this we must set state we are
      ;; registering as current.
      (set-state app state)

      (init state-object)

      (when previous-state (setf current-state previous-state)))))

(defun init-state (app &optional state-name)
  "Initialize state STATE-NAME if provided, current state otherwise."
  (let ((state (if state-name (get-state app state-name) (current-state app)))
        (previous-state current-state))
    ;; NOTE: Temporarily set state as current to call `init' properly.
    (setf (slot-value app 'current-state) state)
    (init state)
    (setf (slot-value app 'current-state) previous-state)))

(defun set-state (app state-name)
  "Set state STATE-NAME as current for APP."
  (let ((state (get-state app state-name)))
    (setf (slot-value app 'current-state) state)))

(defun get-state (app state-name)
  (or (gethash state-name (states app) nil)
      (error "State ~a is not registered." state-name)))

(defmacro continuable (&body body)
  "A macro to disable livesupport when in SLIME"
  #+slynk
  `(livesupport:continuable ,@body)
  #-slynk
  `(progn ,@body))

(defparameter *after-step-callbacks* '())
(defun add-after-step-callback (callback)
  (push callback *after-step-callbacks*))
(defun run-after-step-callbacks ()
  (dolist (c *after-step-callbacks*)
    (funcall c))
  (setf *after-step-callbacks* '()))

(defmethod start ((app application))
  (sdl2-image:init '(:png))
  (sdl2-ttf:init)
  (sdl2:with-init (:video)
    (sdl2:with-window (win :title "o2"
                           :w 1024
                           :h 768)
      (sdl2:with-renderer (ren win :flags '(:accelerated :presentvsync))
        (let* ((renderer (make-instance 'renderer :renderer ren))
               current-frame)

          (set-current-renderer renderer)
          (setf (slot-value app 'renderer) renderer)

          (add-sprite :logo (res/gfx "logo.png"))

          (add-sprite :background (res/gfx "background-placeholder.png"))
          (add-sprite :player (res/gfx "player-pistol-firing.png"))
          (add-sprite :player-sitting (res/gfx "player-sit-pistol-firing.png"))
          (add-sprite :bad-guy (res/gfx "bad-guy-placeholder.png"))
          (add-sprite :G17 (res/gfx "G17.png"))
          (add-sprite :9x19 (res/gfx "9x19.png"))

          (add-font :ubuntu (res/fonts "Ubuntu-R.ttf") :font-size 16)
          (add-font :ubuntu-large (res/fonts "Ubuntu-R.ttf") :font-size 36)

          (register-state app 'main-menu-state)
          (register-state app 'ingame-state)
          (set-state app 'main-menu-state)

          (with-slots ((state current-state)) app
            (continuable
              (sdl2:with-event-loop (:method :poll)
                (:idle ()
                       (setf current-frame (sdl2:get-ticks))
                       ;; TODO move out to :before and :after render

                       #+slynk
                       (livesupport:update-repl-link)

                       (sdl2:render-clear ren)

                       ;; FIXME: this is only called to make a physic world step in ingame,
                       ;;        do something proper here
                       (update state)
                       (with-accessors ((objects objects)) state
                           (dolist (object objects)
                             (run-systems object)))
                       (run-after-step-callbacks)

                       (sdl2:render-present ren)
                       (let ((current-speed (- (sdl2:get-ticks)
                                               current-frame)))
                         (when (< current-speed +delay+)
                           (sdl2:delay (round (- +delay+ current-speed))))))
                (:quit ()
                       ;; Clean up states on shutdown
                       (loop for state being the hash-values of (states *application*)
                             do (cleanup state))
                       (when (not (null *physical-component-cleanups*))
                         ;; Run physics components cleanups
                         (dolist (cleanup *physical-component-cleanups*)
                           (funcall cleanup))
                         (setf *physical-component-cleanups* '()))

                       ;; FIXME: this is UGLY, figure out a way to cleanup all the physics and THEN
                       ;; the space. Maybe some kind of cleanup queue for all objects, that runs all
                       ;; cleanups at the end of the step, so that the program arives here,
                       ;; there would be nothing tat depends on the physics space
                       (let ((ingame-state (gethash :ingame (states *application*))))
                         (when ingame-state
                           (with-slots (space) ingame-state
                               (chipmunk:free-space space))))

                       (setf *application* nil)

                       (sdl2-image:quit)
                       (sdl2-ttf:quit)
                       t)))))))))
