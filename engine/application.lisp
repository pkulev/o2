#| Application representation.

To write application using this engine you should subclass `application'.
|#
(in-package :o2/engine)

(defparameter +delay+ (/ 1000.0 30.0) "FPS")

(defvar *application* nil "Instance of the current application.")


(defclass application ()
  ((frame-ms :initarg :frame-ms
             :accessor frame-ms
             :documentation "Desired time (ms) we can spend in each frame.")

   (states :initarg :states
           :reader states
           :documentation "All states of an application.")

   (current-state :initarg :current-state
                  :reader current-state
                  :documentation "Link to current state (object).")

   ;; TODO: renderer must not be set by hand from client code
   (renderer :initarg :renderer
             :documentation "Link to renderer object."))

  (:default-initargs
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
  "Register new STATE in the application (APP). State will be instantiated."
  (with-slots ((ren renderer) states current-state) app
    (when (gethash state states)
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

(defun deregister-state (app state-name)
  (with-slots (states) app
    (let ((maybe-st (gethash state-name states)))
      (when maybe-st
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
    (setf (slot-value app 'current-state) state)))

(defun get-state (app state-name)
  (or (gethash state-name (states app) nil)
      (error "State ~a is not registered." state-name)))

(defgeneric start (application)
  (:documentation "Start application."))

(defgeneric stop (application)
  (:documentation "Stop application."))
