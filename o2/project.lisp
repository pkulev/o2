#| Implements project lifecycle and project settings.

Project entry can be serialized into some project registry.
(:name "beautiful-project" :path "/path/to/beautiful-project")

Each registered project has it's own configuration file in it's root path.
It may contain some useful settings for preparing builds, i18n, etc.
|#

(in-package :o2/engine)


;; TODO: cross-platform: very naive implementation for early development purposes
(defvar +engine-configuration-files+ (list "./development-settings"
                                           "~/.config/o2/engine/settings"
                                           "/etc/o2engine/settings")
  "List of expected configuration files.")

(defclass project ()
  ((name :initarg name
         :reader name
         :documentation "Project name.")
   (path :initarg path
         :reader reader
         :documentation "Project path."))
  (:default-initargs
   :name (error "Name must be set.")))


(defun create-project (name path)
  "Create new project NAME that lives in PATH."
  (let ((the-project (make-instance 'project :name name :path path)))
    ;; TODO: check path
    ;; TODO: create if not exists
    ;; TODO: write project settings
    ;; TODO: with-registry-lock
    ;; TODO: read registry
    ;; TODO: append to registry
    the-project))

(defun delete-project (name path)
  "Delete project by NAME and PATH."

  (let ((the-project (make-instance 'project :name name :path path)))
    ;; TODO: (obj (discover the-project))
    ;; TODO: (delete obj)
    t))

(defun get-projects ()
  "Get all registered projects."
  ;; TODO: for file in +engine-configuration-files+
  ;; TODO: (name path) = desctructuring-bind
  ;; TODO: collect (make-instance 'project :name name :path path)
  ;; TODO: return collected project
  nil)
