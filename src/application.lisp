(in-package :o2)

(defparameter +delay+ (/ 1000.0 30.0) "FPS")

(defvar *application* nil "Instance of current application.")

(defun res ()
  (merge-pathnames #p"res/" +root+))

(defun res/gfx (file-name)
  (merge-pathnames file-name (merge-pathnames #p"gfx/" (res))))

(defun res/snd (file-name)
  (merge-pathnames file-name (merge-pathnames #p"snd/" (res))))


(defclass application ()
  ((states :initform (make-hash-table)
           :reader states)
   (current-state :initform nil
                  :reader current-state)
   (renderer :initform nil)
   (running? :initform nil)))

(defun make-application ()
  (when *application* (error "Application instance already exists"))
  (setf *application* (make-instance 'application)))

(defun get-current-application ()
  *application*)

(defun current-app-state ()
  (current-state (get-current-application)))

(defun register-state (app state-class state-name)
  (with-slots ((states states)
               (ren renderer)
               current-state) app
    (if (gethash state-name states)
        (error "State ~a already registered" state-name))
    (let ((state (make-instance state-class
                                :application app
                                :name state-name
                                :renderer ren)))
      (unless current-state (setf current-state state))
      (setf (gethash state-name states) state))))

(defun set-state (app state-name &key no-reinit)
  (with-slots ((states states)
               current-state) app
    (let ((state (gethash state-name states)))
      (unless state (error "State ~a is not registered." state-name))
      (setf current-state state)
      (unless no-reinit (init state)))))

(defun get-state (app state-name)
  (or (gethash state-name (states app) nil)
      (error "No state with name ~S" state-name)))

(defmethod start ((app application))
  (sdl2-image:init '(:png))
  (sdl2:with-init (:video)
    (sdl2:with-window (win :title "o2"
                           :w 1024
                           :h 768)
      (sdl2:with-renderer (ren win
                          :flags '(:accelerated :presentvsync))
        (let* ((renderer (make-instance 'renderer :renderer ren))
               current-frame)
          (set-current-renderer renderer)
          (setf (slot-value app 'renderer) renderer)

          (add-sprite :background (res/gfx "background-placeholder.png"))
          (add-sprite :player (res/gfx "player-placeholder.png"))
          (add-sprite :player-sitting (res/gfx "player-sitting-placeholder.png"))
          (add-sprite :bad-guy (res/gfx "bad-guy-placeholder.png"))

          (register-state app 'ingame-state :ingame)
          (set-state app :ingame)

          (with-slots ((state current-state)) app
            (livesupport:continuable (sdl2:with-event-loop (:method :poll)
              (:keydown (:keysym keysym)
                        (process-input state :keydown keysym))
              (:keyup (:keysym keysym)
                      (process-input state :keyup keysym))
              (:idle ()
                     (setf current-frame (sdl2:get-ticks))
                     ;; TODO move out to :before and :after render
                     (livesupport:update-repl-link)
                     (sdl2:render-clear ren)
                     (update state)
                     (render state)
                     (sdl2:render-present ren)
                     (let ((current-speed (- (sdl2:get-ticks)
                                             current-frame)))
                       (when (< current-speed +delay+)
                         (sdl2:delay (round (- +delay+ current-speed))))))
              (:quit ()
                     (sdl2-image:quit)
                     (setf *application* nil)
                     t)))))))))
