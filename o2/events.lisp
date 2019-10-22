#| Event queue routines.

Assume that we can add more backend for events and/or switch default backend.
|#

(in-package :o2/engine)

(defun register-event-type (type)
  "Register new event type."
  (sdl2:register-user-event-type type))

(defun push-event (event &optional user-data)
  "Push EVENT with USER-DATA."
  (sdl2:push-user-event event user-data))
