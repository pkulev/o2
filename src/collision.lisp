(in-package :o2)

(defun rect-collide? (rect1 rect2)
  "Return t if RECT1 and RECT2 collide, nil otherwise."
  (not (or (> (x rect1) (+ (x rect2) (w rect2)))
           (< (+ (x rect1) (w rect1)) (x rect2))
           (> (y rect1) (+ (y rect2) (h rect2)))
           (< (y rect1) (y rect2)))))

