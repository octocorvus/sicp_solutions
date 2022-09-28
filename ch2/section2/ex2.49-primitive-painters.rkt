#lang racket
(require racket/gui/base)

; Make a painter that draws given line segments.
(define (segments->painter segement-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame)
         (start-segment segment))
        ((frame-coord-map frame)
         (end-segment segment))))
     segement-list)))

; Make painter that draws outline of the designated frame.
(define (outline frame)
  (define painter
    (segments->painter
     (list
      (make-segment (make-vect 0 0)
                    (make-vect 1 0))
      (make-segment (make-vect 1 0)
                    (make-vect 1 1))
      (make-segment (make-vect 1 1)
                    (make-vect 0 1))
      (make-segment (make-vect 0 1)
                    (make-vect 0 0)))))
  (painter frame))

; Make painter that draws an "X" by connecting opposite
; corners of the frame.
(define (x-painter frame)
  (define painter
    (segments->painter
     (list
      (make-segment (make-vect 0 0)
                    (make-vect 1 1))
      (make-segment (make-vect 0 1)
                    (make-vect 1 0)))))
  (painter frame))

; Make painter that draws diamond shape by connecting
; the midpoints of the sides of the frame.
(define (diamond frame)
  (define painter
    (segments->painter
     (list
      (make-segment (make-vect 0.5 0)
                    (make-vect 1 0.5))
      (make-segment (make-vect 1 0.5)
                    (make-vect 0.5 1))
      (make-segment (make-vect 0.5 1)
                    (make-vect 0 0.5))
      (make-segment (make-vect 0 0.5)
                    (make-vect 0.5 0)))))
  (painter frame))

; Make the wave painter.
(define (wave frame)
  (define painter
    (segments->painter
     (list
      (make-segment (make-vect 0 0.65)
                    (make-vect 0.18 0.4666666666666666))
      (make-segment (make-vect 0.18 0.4666666666666666)
                    (make-vect 0.38 0.5666666666666667))
      (make-segment (make-vect 0.38 0.5666666666666667)
                    (make-vect 0.26 0))
      (make-segment (make-vect 0.38 0)
                    (make-vect 0.52 0.31666666666666665))
      (make-segment (make-vect 0.52 0.31666666666666665)
                    (make-vect 0.6599999999999999 0))
      (make-segment (make-vect 0.78 0)
                    (make-vect 0.62 0.5666666666666667))
      (make-segment (make-vect 0.62 0.5666666666666667)
                    (make-vect 1.0 0.2833333333333333))
      (make-segment (make-vect 1.0 0.3833333333333333)
                    (make-vect 0.7 0.6833333333333332))
      (make-segment (make-vect 0.7 0.6833333333333332)
                    (make-vect 0.58 0.65))
      (make-segment (make-vect 0.58 0.65)
                    (make-vect 0.64 0.8333333333333334))
      (make-segment (make-vect 0.64 0.8333333333333334)
                    (make-vect 0.58 1.0))
      (make-segment (make-vect 0.42000000000000004 1.0)
                    (make-vect 0.33999999999999997 0.8333333333333334))
      (make-segment (make-vect 0.33999999999999997 0.8333333333333334)
                    (make-vect 0.42000000000000004 0.65))
      (make-segment (make-vect 0.42000000000000004 0.65)
                    (make-vect 0.32 0.6833333333333332))
      (make-segment (make-vect 0.32 0.6833333333333332)
                    (make-vect 0.2 0.5833333333333334))
      (make-segment (make-vect 0.2 0.5833333333333334)
                    (make-vect 0 0.75)))))
  (painter frame))

; We represent a directed line segment in a plane
; as a pair of vectors -- the vector running from
; the origin to the start-point of the segment,
; and the vector running from the origin to the
; end-point of the segment.
(define (make-segment start-vect end-vect)
  (list start-vect end-vect))

(define (start-segment segment)
  (list-ref segment 0))

(define (end-segment segment)
  (list-ref segment 1))

(define (make-vect x y)
  (list x y))

(define (xcor-vect v)
  (list-ref v 0))

(define (ycor-vect v)
  (list-ref v 1))

; Add two vectors.
; (x1,y1)+(x2,y2)=(x1+x2,y1+y2).
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1)
                (xcor-vect v2))
             (+ (ycor-vect v1)
                (ycor-vect v2))))

; Subtract two vectors.
; (x1,y1)+(x2,y2)=(x1-x2,y1-y2)
(define (sub-vect v1 v2)
  (add-vect v1 (additive-inverse v2)))

; Additive inverse of a vector.
; AdditiveInverse((x,y))=(-x,-y)
(define (additive-inverse v)
  (make-vect (- (xcor-vect v))
             (- (ycor-vect v))))

; Scale a vector.
; s*(x,y)=(s*x,s*y)
(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (list-ref frame 0))

(define (edge1-frame frame)
  (list-ref frame 1))

(define (edge2-frame frame)
  (list-ref frame 2))

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

; Drawing on the screen.
; Our origin.
(define origin (make-vect 250 250))

; Draw a line from first point to second point.
; Points are represented as vectors.
(define (draw-line v1 v2)
  (let ((p1 (add-vect origin
                      (make-vect (xcor-vect v1)
                                 (- (ycor-vect v1)))))
        (p2 (add-vect origin
                      (make-vect (xcor-vect v2)
                                 (- (ycor-vect v2))))))
    (let ((x1 (xcor-vect p1))
          (y1 (ycor-vect p1))
          (x2 (xcor-vect p2))
          (y2 (ycor-vect p2)))
      (send (send canvas get-dc) draw-line x1 y1 x2 y2))))

; Clear the screen contents.
(define (clear)
  (send canvas refresh))

; The main frame.
(define main-frame (new frame%
                   [label "Picture Language"]
                   [style (list 'no-resize-border
                                'float)]
                   [min-width (* 2 (xcor-vect origin))]
                   [min-height (* 2 (ycor-vect origin))]
                   [stretchable-width #f]
                   [stretchable-height #f]))

; Canvas to draw stuff on.
(define canvas (new canvas%
                    [parent main-frame]
                    [paint-callback
                     (lambda (canvas dc)
                       (send dc set-smoothing 'smoothed)
                       (send dc set-pen "white" 1 'solid)
                       (send dc set-brush "white" 'solid)
                       (send dc set-text-foreground "white")
                       (send dc draw-text "Origin"
                             (+ 2 (xcor-vect origin))
                             (+ 2 (ycor-vect origin)))
                       (let ((size 4))
                         (send dc
                               draw-ellipse
                               (- (xcor-vect origin) (/ size 2))
                               (- (ycor-vect origin) (/ size 2))
                               size size)))]))

; Set background color of the canvas.
(send canvas
      set-canvas-background
      (make-object color% 10 10 15))

(send main-frame show #t)

; Wait for the window to get ready.
(sleep/yield 0.001)

; Now draw stuff.
(define frame1 (make-frame (make-vect 12 -59)
                           (make-vect 69 42)
                           (make-vect 55 -150)))

(outline frame1)
(x-painter frame1)
(diamond frame1)
(wave frame1)
