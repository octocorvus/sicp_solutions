#lang sicp

(define (frame-coord-map frame)
  ; Because of data abstraction we don't actually
  ; care about how vectors or heck even frames are
  ; represented, so long their operations behave
  ; correctly.
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect
      (scale-vect (xcor-vect v)
                  (edge1-frame frame))
      (scale-vect (ycor-vect v)
                  (edge2-frame frame))))))
