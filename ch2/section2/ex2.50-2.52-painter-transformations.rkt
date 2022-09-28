#lang racket
(require racket/gui/base)

(define (transform-painter
         painter origin corner1 corner2)
  (lambda (frame)
    (painter (transform-frame
              frame origin corner1 corner2))))

; Exercise 2.50
; Flip painter images horizontally.
(define (flip-horiz painter)
  (transform-painter
   painter
   (make-vect 1.0 0.0)
   (make-vect 0.0 0.0)
   (make-vect 1.0 1.0)))

; Flip painter images vertically.
(define (flip-vert painter)
  (transform-painter
   painter
   (make-vect 0.0 1.0)
   (make-vect 1.0 1.0)
   (make-vect 0.0 0.0)))

; Shrink painter image to the upper right quarter of
; the frame.
(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

; Rotate painter image counterclockwise by 90 degrees.
(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

; Exercise 2.50
; Rotate painter image counterclockwise by 180 degrees.
(define (rotate180 painter)
  (rotate90 (rotate90 painter)))

; Exercise 2.50
; Rotate painter image counterclockwise by 270 degress.
(define (rotate270 painter)
  (rotate90 (rotate180 painter)))

; Squash painter image to the center of the frame.
(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

; Draw painter1 and painter2 in the left half and the
; right half of a frame respectively.
(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    ; paint-left: painter that draws painter1 in the
    ;   left half of the frame.
    ; paint-right: painter that draws painter2 in the
    ;   right half of the frame.
    (let ((paint-left (transform-painter
                       painter1
                       (make-vect 0.0 0.0)
                       split-point
                       (make-vect 0.0 1.0)))
          (paint-right (transform-painter
                        painter2
                        split-point
                        (make-vect 1.0 0.0)
                        (make-vect 0.5 1.0))))
      ; Return a compound painter that takes a frame and
      ; draws paint-left and paint-right in the frame.
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

; Exercise 2.51
; Draw painter1 image below painter2 image. In other
; words, draw painter1 and painter2 in the lower half
; and the upper half of a frame respectively.
(define (below painter1 painter2)
  ; Rotate both painters by 90 degrees counterclockwise,
  ; them beside each other, then rotate them by 270
  ; degrees counterclockwise.
  (rotate270 (beside (rotate90 painter2)
                     (rotate90 painter1))))


(define (split first second)
  (define (split-impl painter n)
    (if (= n 0)
        painter
        (let ((smaller (split-impl painter
                                   (- n 1))))
          (first painter
                 (second smaller smaller)))))
  (lambda (painter n)
    (split-impl painter n)))

(define right-split (split beside below))

(define up-split (split below beside))

(define vert-split (split beside (lambda (x y) x)))

(define horiz-split (split below (lambda (x y) x)))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((top-left (horiz-split painter
                                   (- n 1)))
            (bottom-right (vert-split painter
                                      (- n 1)))
            (corner (corner-split painter
                                  (- n 1))))
        (beside (below painter top-left)
                (below bottom-right
                       corner)))))

; Produce a painter operation that transform a given
; painter with the following operations and arranges
; the result in a square:
; tl: transformation to apply on top-left part of image
; tr: transformation to apply on top-right part of image
; bl: transformation to apply on bottom-left part of image
; br: transformation to apply on bottom-right part of image
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter)
                       (tr painter)))
          (bottom (beside (bl painter)
                          (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  (let ((combine4
         (square-of-four flip-vert
                         rotate180
                         identity
                         flip-horiz)))
    (combine4 (corner-split painter n))))

(define (flipped-pairs painter)
  (let ((combine4
         (square-of-four identity
                         flip-vert
                         identity
                         flip-vert)))
    (combine4 painter)))

; Transform a frame. The arguments are the frame and
; points that specify corners of the new frame: When
; mapped into the frame the first point specify the
; new frame's origin and the other two specify the
; end of its edge vectors.
(define (transform-frame
         frame origin corner1 corner2)
  (let ((m (frame-coord-map frame)))
    (let ((new-origin (m origin)))
      (make-frame new-origin
                  (sub-vect (m corner1)
                            new-origin)
                  (sub-vect (m corner2)
                            new-origin)))))

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
      ; Face outline.
      (make-segment (make-vect 0.4 0.65)
                    (make-vect 0.36 0.83))
      (make-segment (make-vect 0.36 0.83)
                    (make-vect 0.42 1.0))
      (make-segment (make-vect 0.58 1.0)
                    (make-vect 0.64 0.83))
      (make-segment (make-vect 0.64 0.83)
                    (make-vect 0.60 0.65))
      ; Right hand.
      (make-segment (make-vect 0.60 0.65)
                    (make-vect 0.70 0.68))
      (make-segment (make-vect 0.70 0.68)
                    (make-vect 1.0 0.38))
      (make-segment (make-vect 1.0 0.28)
                    (make-vect 0.62 0.56))
      ; Left hand.
      (make-segment (make-vect 0.4 0.65)
                    (make-vect 0.30 0.68))
      (make-segment (make-vect 0.30 0.68)
                    (make-vect 0.2 0.58))
      (make-segment (make-vect 0.2 0.58)
                    (make-vect 0 0.75))
      (make-segment (make-vect 0 0.65)
                    (make-vect 0.18 0.467))
      (make-segment (make-vect 0.18 0.467)
                    (make-vect 0.38 0.56))
      ; Right leg.
      (make-segment (make-vect 0.62 0.56)
                    (make-vect 0.78 0))
      (make-segment (make-vect 0.66 0)
                    (make-vect 0.5 0.316))
      ; Left leg.
      (make-segment (make-vect 0.38 0.56)
                    (make-vect 0.26 0))
      (make-segment (make-vect 0.38 0)
                    (make-vect 0.5 0.316))
      ; Exercise 2.52.1
      ; Smile.
      (make-segment (make-vect 0.58 0.8)
                    (make-vect 0.55 0.767))
      (make-segment (make-vect 0.55 0.767)
                    (make-vect 0.5 0.754))
      (make-segment (make-vect 0.5 0.754)
                    (make-vect 0.45 0.767))
      (make-segment (make-vect 0.45 0.767)
                    (make-vect 0.42 0.8))
      ; Right eye.
      (make-segment (make-vect 0.52 0.88)
                    (make-vect 0.57 0.88))
      (make-segment (make-vect 0.57 0.88)
                    (make-vect 0.57 0.9))
      (make-segment (make-vect 0.57 0.9)
                    (make-vect 0.52 0.9))
      (make-segment (make-vect 0.52 0.9)
                    (make-vect 0.52 0.88))
      ; Left eye.
      (make-segment (make-vect 0.48 0.88)
                    (make-vect 0.43 0.88))
      (make-segment (make-vect 0.43 0.88)
                    (make-vect 0.43 0.9))
      (make-segment (make-vect 0.43 0.9)
                    (make-vect 0.48 0.9))
      (make-segment (make-vect 0.48 0.9)
                    (make-vect 0.48 0.88)))))
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
(define origin (make-vect 400 250))

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
(define frame1 (make-frame (make-vect -150 -59)
                           (make-vect 69 42)
                           (make-vect 55 -150)))

(outline frame1)
((shrink-to-upper-right outline) frame1)
((flip-vert (shrink-to-upper-right wave)) frame1)

(define frame2
  (make-frame (make-vect -140 0)
              (make-vect -200 0)
              (make-vect -50 -150)))

(outline frame2)
(x-painter frame2)
(diamond frame2)
((flip-vert wave) frame2)

(define frame3
  (make-frame (make-vect -180 10)
              (make-vect 200 0)
              (make-vect 0 200)))

(outline frame3)
((squash-inwards outline) frame3)
((squash-inwards wave) frame3)

(define frame4
  (make-frame (make-vect -390 10)
              (make-vect 200 0)
              (make-vect 0 200)))

(outline frame4)
((shrink-to-upper-right
  (beside wave
          (flip-horiz wave))) frame4)
((rotate90
  (shrink-to-upper-right
   (beside wave
           (flip-horiz wave)))) frame4)
((rotate180
  (shrink-to-upper-right
   (beside wave
           (flip-horiz wave)))) frame4)
((rotate270
  (shrink-to-upper-right
   (beside wave
           (flip-horiz wave)))) frame4)

(define frame5
  (make-frame (make-vect 30 0)
              (make-vect 100 0)
              (make-vect 0 150)))

(outline frame5)
((below wave (flip-vert wave)) frame5)

(define frame6
  (make-frame (make-vect -20 -230)
              (make-vect 200 0)
              (make-vect 0 200)))

(outline frame6)
((flipped-pairs wave) frame6)

(define frame7
  (make-frame (make-vect 150 0)
              (make-vect 200 0)
              (make-vect 0 200)))

(outline frame7)
((square-limit wave 4) frame7)
