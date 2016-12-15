#lang racket
(require 2htdp/image 2htdp/universe)
(require racket/date)

;'(0 1 2 3 4 5 6 7 8 9)
(define digit-images
  (map (lambda (i) (text (number->string i) 48 'black)) (range 10)))

(define WIDTH (image-width (list-ref digit-images 0)))
(define HEIGHT (image-height (list-ref digit-images 0)))
(define TRANSPARENT (color 255 255 255 0))

(define (blank? acolor)
  (equal? TRANSPARENT acolor))

;0-9 as long list of pixels
(define digit-pixels (map image->color-list digit-images))

(struct pt (x y c) #:transparent)

(define (pixels->pts pixels)
  (for/list ([i (length pixels)]
             #:unless (blank? (list-ref pixels i)))
    (pt (remainder i WIDTH) (quotient i WIDTH) (list-ref pixels i))))

(define digit-pts (map pixels->pts digit-pixels))

;
; DRAWING
;

(define frame-background (rectangle WIDTH HEIGHT 'solid 'white))

(define (draw-pt apt ascene)
  (place-image (circle 1 'solid (pt-c apt)) (pt-x apt) (pt-y apt) ascene))

(define (draw-frame points)
  (foldl draw-pt frame-background points))

(define (draw-clock state)
  (let* ([seconds (date-second (current-date))]
         [ones-place (modulo seconds 10)]
         [ones-next (modulo (add1 ones-place) 10)]
         [tens-place (quotient seconds 10)]
         [tens-next (quotient (add1 tens-place) 10)])
  (draw-frame
   (lerp-frame
    (list-ref digit-pts ones-place)
    (list-ref digit-pts ones-next)
    state))))

;
; LERPING
;

(define (lerp orig targ alpha)
  (+ (* (- 1 alpha) orig) (* alpha targ)))

(define (lerp-pt opt tpt alpha)
  (pt
   (lerp (pt-x opt) (pt-x tpt) alpha)
   (lerp (pt-y opt) (pt-y tpt) alpha)
   (color
    (round (lerp (color-red (pt-c opt)) (color-red (pt-c tpt)) alpha))
    (round (lerp (color-green (pt-c opt)) (color-green (pt-c tpt)) alpha))
    (round (lerp (color-blue (pt-c opt)) (color-blue (pt-c tpt)) alpha))
    (round (lerp (color-alpha (pt-c opt)) (color-alpha (pt-c tpt)) alpha)))))

(define zero-pts (list-ref digit-pts 0))
(define one-pts (list-ref digit-pts 1))

(define (equalize orig targ)
  (if (> (length orig) (length targ))
      orig
      (append (take (shuffle orig) (- (length targ) (length orig))) orig)))

(define (random-pt x)
  (pt (random WIDTH) (random HEIGHT) TRANSPARENT))

(define (equalize2 orig targ)
  (if (> (length orig) (length targ))
      orig
      (append (build-list (- (length targ) (length orig)) random-pt) orig)))


(define (lerp-frame opts tpts alpha)
  (map (lambda (o t)
         (lerp-pt o t alpha)) (equalize opts tpts) (equalize tpts opts))) 

;
; TICKING
;

(define (tick-handler state)
  (exact->inexact (/ (date*-nanosecond (current-date)) 1000000000)))

;
; UNIVERSE
;

(big-bang 0.0
          (name "Pixelclock")
          (on-draw draw-clock)
          (on-tick tick-handler)
          )
