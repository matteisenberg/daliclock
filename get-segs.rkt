#lang racket
(require 2htdp/image)

;'(0 1 2 3 4 5 6 7 8 9)
(define digit-images
  (map (lambda (i) (text (number->string i) 96 'black)) (range 10)))

(define WIDTH (image-width (list-ref digit-images 0)))
(define HEIGHT (image-height (list-ref digit-images 0)))
(define TRANSPARENT (color 255 255 255 0))

;0-9 as long list of pixels
(define digit-pixels (map image->color-list digit-images))

(define (make-table width height somepixels)
  (for/list ([row (range height)])
    (for/list ([col (range width)])
      (list-ref somepixels (+ (* width row) col)))))

(define zero-table
  (make-table WIDTH HEIGHT (list-ref digit-pixels 0)))

(define one-table
  (make-table WIDTH HEIGHT (list-ref digit-pixels 1)))

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
    (append b a)))

(define zero-segs (map get-segs zero-table))
(define one-segs (map get-segs one-table))

(define (draw-segment asegment ascene arow)
  (scene+line ascene
              (segment-start asegment)
              arow
              (segment-end asegment)
              arow
              (pen (color 0 0 0 255) 2 'solid 'butt 'miter)))

(define frame-background (rectangle 53 96 'solid 'white))

(define (draw-frame aframe)
  (for/fold
   ([img frame-background])
   ([row (length aframe)])
    (foldl (lambda (seg scene) (draw-segment seg scene row))
           img
           (list-ref aframe row))))

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

(define (lerp-one-to-zero alpha)
    (draw-frame (lerp-frame zero-segs one-segs alpha)))

(map lerp-one-to-zero (range 0 1.0 .1))