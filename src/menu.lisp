(in-package :o2)

(defclass main-menu-state (state)
  ((choice :initform 0
           :accessor choice)))

(defclass menu-choice-c (component)
  ((choice-number :initarg :choice-number
                  :accessor choice-number)
   (text :initarg :text
         :accessor text)
   (dimensions :initarg :dimensions
               :accessor dimensions)
   (color :initarg :color
          :accessor color
          :documentation "Color in the '(R G B A) format")
   (selected-color :initarg :selected-color
                   :accessor selected-color
                   :documentation "Color when selected, in the '(R G B A) format")
   (action :initarg :action
           :accessor action)))

(defclass menu-choice-system (system)
  ((requires :initform '(menu-choice-c transform-c))))

(defmethod run-system ((system menu-choice-system) found-components)
  (destructuring-bind (choice tr) found-components
    (with-slots ((current-choice choice)) (current-app-state)
      (with-accessors ((pos position)) tr
        (with-accessors ((button-choice choice-number) (dims dimensions)
                         (color color) (sel-color selected-color)
                         (text text))
            choice
          (draw-rect pos dims
                     (if (= current-choice button-choice)
                         sel-color
                         color))
          (draw-text :ubuntu-large text (car pos) (cdr pos)
                     :color '(255 255 255)))))))

(defclass menu-system (system)
  ((requires :initform '(transform-c))))

(defmethod run-system ((system menu-system) found-components)
  (destructuring-bind (tr) found-components
    (with-accessors ((children children)) tr    
      (cond
        ((sdl2:keyboard-state-p :scancode-escape)
         (sdl2:push-event :quit))
        ((sdl2:keyboard-state-p :scancode-return)
         (loop named buttons
               for butt in children
               do (let ((butt-c (find-component butt 'menu-choice-c)))
                    (when (= (choice-number butt-c) (choice (current-app-state)))
                      (funcall (action butt-c))
                      (return-from buttons)))))
        ;; FIXME: BIG HACK FOR TWO BUTTONS ONLY, FIX
        ;; FIXME: add a key-repeat delay
        ((sdl2:keyboard-state-p :scancode-down)
         (with-slots (choice) (current-app-state)
           (setf choice (mod (1+ choice) 2))))
        ((sdl2:keyboard-state-p :scancode-up)
         (with-slots (choice) (current-app-state)
           (setf choice (mod (1- choice) 2))))))))

(defmethod init ((menu main-menu-state))
  ;; A static camera
  (add-object
   menu
   (make-instance
    'game-object
    :components (list
                 (make-instance 'transform-c)
                 (make-instance 'camera-tag))))

  ;; The background image
  (add-object
   menu
   (make-instance
    'game-object
    :components
    (list
     (make-instance 'transform-c :position '(-512 . -385))
     (make-instance 'render-c :sprite :background
                              :render-priority 0))
    :systems
    (list
     (make-instance 'render-system))))

  (add-object
   menu
   (make-instance 'game-object
                  :components
                  (list
                   (make-instance 'render-c :sprite :logo
                                            :render-priority 1)
                   (make-instance 'transform-c :position '(150 . 100)))
                  :systems (list
                            (make-instance 'render-system))))
  
  (let* ((menu-object
           (add-object
            menu
            (make-instance 'game-object
                           :components
                           (list                            
                            (make-instance 'transform-c))
                           :systems
                           (list
                            (make-instance 'menu-system)))))
         (button-w 500)
         (button-h 100)
         (base-butt-x (- (/ 1024 2) (/ button-w 2)))
         (base-butt-y (+ (/ 768 2) (/ button-h 2))))

    (add-child
          menu-object
          (add-object
           menu
           (make-instance
            'game-object
            :components (list
                         (make-instance 'transform-c
                                        :position (cons base-butt-x base-butt-y))
                         (make-instance 'render-c
                                        :render-priority 1)
                         (make-instance 'menu-choice-c
                                        :color '(29 149 182 128)
                                        :selected-color '(17 95 118 192)
                                        :dimensions (cons button-w button-h)
                                        :choice-number 0
                                        :text "New game"
                                        :action (lambda ()
                                                  (add-after-step-callback
                                                   (lambda ()
                                                     (set-state *application* :ingame))))))
            :systems (list
                      (make-instance 'menu-choice-system)))))
    (add-child
     menu-object
     (add-object
      menu
      (make-instance
       'game-object
       :components (list
                    (make-instance 'transform-c
                                   :position (cons base-butt-x
                                                   (+ base-butt-y button-h 25)))
                    (make-instance 'render-c
                                   :render-priority 1)
                    (make-instance 'menu-choice-c
                                   :color '(29 149 182 128)
                                   :selected-color '(17 95 118 192)
                                   :dimensions (cons button-w button-h)
                                   :choice-number 1
                                   :text "Exit"
                                   :action (lambda ()
                                               (sdl2:push-event :quit))))
       :systems (list
                 (make-instance 'menu-choice-system)))))))
