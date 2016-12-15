#lang racket
(require 2htdp/image 2htdp/universe)
(require racket/date)

;'(0 1 2 3 4 5 6 7 8 9)
(define digit-images
  (map (lambda (i) (text (number->string i) 96 'black)) (range 10)))

(define WIDTH (image-width (list-ref digit-images 0)))
(define HEIGHT (image-height (list-ref digit-images 0)))
(define TRANSPARENT (color 255 255 255 0))

;0-9 as long list of pixels
(define digit-pixels (map image->color-list digit-images))

;organize the long list of pixels into a table of rows
(define (make-table width height somepixels)
  (for/list ([row (range height)])
    (for/list ([col (range width)])
      (list-ref somepixels (+ (* width row) col)))))

(define digit-tables (map
                      (lambda (pixels) (make-table WIDTH HEIGHT pixels))
                      digit-pixels))

(define zero-table (list-ref digit-tables 0))
(define one-table (list-ref digit-tables 1))

(define (blank? acolor)
  (equal? TRANSPARENT acolor))

(struct segment (start end) #:transparent)

(define (get-segs alist)
  (let-values ([(a b)
                (for/fold
                 ;accumulators
                 ([seg '()]
                  [segments '()])
                 ;iterator(s)
                 ([pos (length alist)])
                  ;body
                  (let*
                      ([e (list-ref alist pos)]
                       [blank (blank? e)])
                    (cond
                      [(and (null? seg) blank) (values seg segments)]
                      [(and (null? seg) (not blank)) (values (segment pos pos) segments)]
                      [(and (not (null? seg)) blank) (values '() (append segments (list seg)))]
                      [(and (not (null? seg)) (not blank)) (values (segment (segment-start seg) pos) segments)])))])
    (flatten(append b a))))

(define digit-segs (for/list ([table (length digit-tables)])
    (map get-segs (list-ref digit-tables table))))

(define zero-segs (list-ref digit-segs 0))
(define one-segs (list-ref digit-segs 1))

;
; LERPING
;

(define (lerp orig targ alpha)
  (+ (* (- 1 alpha) orig) (* alpha targ)))

(define (lerp-segment oseg tseg alpha)
  (segment
   (lerp (segment-start oseg) (segment-start tseg) alpha)
   (lerp (segment-end oseg) (segment-end tseg) alpha)))

(define (lerp-segment-alpha alpha)
  (lambda (orig targ) (lerp-segment orig targ alpha)))

(define (equalize orig targ)
  (cond
    ;can we do better?
    [(null? targ) '()]
    [(null? orig) '()]
    [(or (eq? (length orig) (length targ)) (> (length orig) (length targ))) orig]
    [else (make-list (length targ) (car orig))]))

(define (lerp-frame oframe tframe alpha)
  (for/list
      ([row (range HEIGHT)])
    (let ([i (list-ref oframe row)]
          [j (list-ref tframe row)])
      (map (lerp-segment-alpha alpha) (equalize i j) (equalize j i)))))

;
; DRAWING
;

(define frame-background (rectangle WIDTH HEIGHT 'solid 'white))

(define (draw-segment asegment ascene arow)
  (scene+line ascene
              (segment-start asegment)
              arow
              (segment-end asegment)
              arow
              (pen (color 0 0 0 255) 2 'solid 'butt 'miter)))

(define (draw-frame aframe)
  (for/fold
   ([img frame-background])
   ([row (length aframe)])
    (foldl (lambda (seg scene) (draw-segment seg scene row))
           img
           (list-ref aframe row))))

(define (draw-clock state)
  (let* ([seconds (date-second (current-date))]
         [ones-place (modulo seconds 10)]
         [ones-next (modulo (add1 ones-place) 10)]
         [tens-place (quotient seconds 10)]
         [tens-next (quotient (add1 tens-place) 10)])
  (draw-frame
   (lerp-frame
    (list-ref digit-segs ones-place) (list-ref digit-segs ones-next) state))))

;
; TICKING
;

(define (tick-handler state)
  (exact->inexact (/ (date*-nanosecond (current-date)) 1000000000)))


;
; TESTING - TODO Rackunit!
;

;(define (lerp-one-to-zero alpha)
    ;(draw-frame (lerp-frame (list-ref digit-segs 9) (list-ref digit-segs 0) alpha)))

;(map lerp-one-to-zero (range 0 1.0 .1))




;
; UNIVERSE
;

(big-bang 0.0
          (name "Daliclock")
          (on-draw draw-clock)
          (on-tick tick-handler)
          )


;
; TESTING - TODO Rackunit!
;

;(define (lerp-one-to-zero alpha)
    ;(draw-frame (lerp-frame (list-ref digit-segs 9) (list-ref digit-segs 0) alpha)))

;(map lerp-one-to-zero (range 0 1.0 .1))

