#| Application representation.

To write application using this engine you should subclass `application'.
Application operates hashmap of states. State represents bucket of game objects.
State can be used for level representation for example.
|#
(in-package :o2/engine)


(defvar *application* nil "Instance of the current application.")

(defun get-current-application ()
  "Return an instance of current application."
  *application*)


(defclass application ()
  ((title :initarg :title
          :reader title
          :documentation "Application window title.")

   (width :initarg :width
          :accessor width
          :documentation "Window's width.")

   (height :initarg :height
           :accessor height
           :documentation "Window's height.")

   (frame-ms :initarg :frame-ms
             :accessor frame-ms
             :documentation "Desired time (ms) we can spend in each frame.")

   (states :initarg :states
           :reader states
           :documentation "All states of an application.")

   (current-state :initarg :current-state
                  :reader current-state
                  :documentation "Link to current state (object).")

   (window :reader window
           :documentation "Link to window object.")

   (renderer :reader renderer
             :documentation "Link to renderer object."))

  (:default-initargs
   :title "o2 untitled application"
   :width 1024
   :height 768
   :frame-ms (/ 1000.0 30.0)
   :states (make-hash-table)
   :current-state nil))

(defmethod initialize-instance :after ((app application) &key &allow-other-keys)
  "Store initialized application object in global variable."
  (setf *application* app))

(defun current-app-state ()
  "Return current state of the application."
  (current-state *application*))

(defun register-state (app state)
  "Register new STATE in the application (APP).

State will be instantiated and returned as result."
  (with-slots (states current-state) app
    (when (gethash state states)
      (error "State ~a is already registered." state))

    (log:info "registering state" state)

    (let* ((state-object (make-instance state :application app))
           (previous-state current-state))

      (setf (gethash state states) state-object)
      ;; NOTE: Accessing current state on `init'.
      ;; game objects in state's `init' method can access it's state via
      ;; `current-app-state'. To provide this we must set state we are
      ;; registering as current.
      (set-state app state)

      (init state-object)

      (when previous-state (setf current-state previous-state))
      state-object)))

(defun deregister-state (app state-name)
  "Deregister state STATE-NAME from APP."
  (with-slots (states) app
    (let ((maybe-st (gethash state-name states)))
      (when maybe-st

        (log:info "deregistering state" state-name)

        ;; FIXME: cleanup? this breaks stuff somehow, because
        ;;        removing the body from the space twice is not allowed
        ;; (cleanup maybe-st)
        (setf (gethash state-name states) nil)))))

;; TODO: unused
(defun init-state (app &optional state-name)
  "Initialize state STATE-NAME if provided, current state otherwise."
  (let ((state (if state-name (get-state app state-name) (current-state app)))
        (previous-state (current-state app)))
    ;; NOTE: Temporarily set state as current to call `init' properly.
    (setf (slot-value app 'current-state) state)
    (init state)
    (setf (slot-value app 'current-state) previous-state)))

(defun set-state (app state-name)
  "Set state STATE-NAME as current for APP."
  (let ((state (get-state app state-name)))
    (log:debug "setting" state "for" app "as current")
    (setf (slot-value app 'current-state) state)))

(defun get-state (app state-name)
  (or (gethash state-name (states app) nil)
      (error "State ~a is not registered." state-name)))

(defgeneric init (application)
  (:documentation "Initialize all subsystems that are needed for application."))

(defmethod init :before ((app application))
  (log:info "initializing subsystems"))

(defmethod init ((app application))
  (sdl2:init :video :timer)
  (sdl2-image:init '(:png))
  (sdl2-ttf:init)

  (setf (slot-value app 'window)
        (sdl2:create-window :title (title app)
                            :w (width app)
                            :h (height app)))

  (setf (slot-value app 'renderer)
        (sdl2:create-renderer (window app) nil '(:accelerated :presentvsync))))

(defgeneric deinit (application)
  (:documentation "Destruct all used subsystems."))

(defmethod deinit :before ((app application))
  (log:info "destructing subsystems"))

(defmethod deinit ((app application))
  (sdl2-image:quit)
  (sdl2-ttf:quit)

  (sdl2:destroy-renderer (renderer app))
  (sdl2:destroy-window (window app))

  (sdl2:quit))

(defgeneric start (application)
  (:documentation "Start application."))

(defmethod start :before ((app application))
  (log:info "starting the application"))

(defmethod start ((app application))
  ;; TODO: we should do something useful here, not in the client code
  t)

(defgeneric stop (application)
  (:documentation "Stop application."))

(defmethod stop :before ((app application))
  (log:info "stopping the application"))
