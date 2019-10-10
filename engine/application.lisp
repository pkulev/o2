(in-package :o2/engine)

(defparameter +delay+ (/ 1000.0 30.0) "FPS")

(defvar *application* nil "Instance of the current application.")


(defclass application ()
  ((states :initform (make-hash-table)
           :reader states)
   (current-state :initform nil
                  :reader current-state)
   (renderer :initform nil
             :initarg :renderer)))

(defmethod initialize-instance :after ((app application) &key &allow-other-keys)
  (setf *application* app))

(defun current-app-state ()
  (current-state *application*))

(defun deregister-state (app state-name)
  (with-slots (states) app
    (let ((maybe-st (gethash state-name states)))
      (when maybe-st
        ;; FIXME: cleanup? this breaks stuff somehow, because
        ;;        removing the body from the space twice is not allowed
        ;; (cleanup maybe-st)
        (setf (gethash state-name states) nil)))))

(defun register-state (app state)
  (with-slots ((ren renderer) states current-state) app
    (if (gethash state states)
        (error "State ~a is already registered." state))

    (let* ((state-object (make-instance state :application app))
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
        (previous-state (current-state app)))
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

(defmethod start (application))

(defmethod stop (application))
