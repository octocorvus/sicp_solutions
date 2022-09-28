#lang sicp

; Suppose we have the procedure draw-line that draws
; a line on the screen between two specified points.
; Then we can create painters for line drawings from
; lists of line segements as follows:
; We use representation of line-segments described
; in Exercise 2.48.
; We also use the procedure for-each that is similar
; to map but instead of creating lists of results
; it only applies the given procedure to each of the
; elements in the list.
; The segments are given using coordinates with
; respect to the unit square. For each segment in
; the list, the painter transforms the segment
; endpoints with the frame coordinate map and draws
; a line between the transformed points.
(define (segements->painter segement-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame)
         (start-segment segement))
        ((frame-coord-map frame)
         (end-segment segment))))
     segement-list)))
