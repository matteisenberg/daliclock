#lang racket

(require 2htdp/image)

(define zero (text "0" 48 'black))
(define one (text "1" 48 'black))

(define digits
  (map
   (lambda (digit)
     (text (number->string digit) 48 'black))
   (range 10)))

(define (slice image)
  (map
   (lambda (row)
     (crop 0 row (image-width image) 1 image)) (range (image-height image))))

(define (slice-at image row)
  (crop 0 row (image-width image) 1 image))

(define (dice image)
  (map
   (lambda (col)
     (crop col 0 1 (image-height image) image)) (range (image-width image))))

(define (get-pixel x y image)
  (first (image->color-list (crop x y 1 1 image))))

;transparent white is blank
(define blank (color 255 255 255 0))

(define (blank? acolor)
  (equal? (color 255 255 255 0) acolor))

;zero - row 10
(define orig (vector
 (color 255 255 255 0)
 (color 255 255 255 0)
 (color 255 255 255 0)
 (color 0 0 0 188)
 (color 0 0 0 255)
 (color 0 0 0 255)
 (color 0 0 0 255)
 (color 0 0 0 230)
 (color 0 0 0 14)
 (color 255 255 255 0)
 (color 255 255 255 0)
 (color 255 255 255 0)
 (color 255 255 255 0)
 (color 255 255 255 0)
 (color 255 255 255 0)
 (color 255 255 255 0)
 (color 255 255 255 0)
 (color 255 255 255 0)
 (color 0 0 0 134)
 (color 0 0 0 255)
 (color 0 0 0 255)
 (color 0 0 0 255)
 (color 0 0 0 243)
 (color 0 0 0 9)
 (color 255 255 255 0)
 (color 255 255 255 0)
 (color 255 255 255 0)))

;one - row 10
(define targ (vector
 (color 255 255 255 0)
 (color 255 255 255 0)
 (color 255 255 255 0)
 (color 255 255 255 0)
 (color 0 0 0 103)
 (color 0 0 0 255)
 (color 0 0 0 255)
 (color 0 0 0 255)
 (color 0 0 0 255)
 (color 0 0 0 255)
 (color 0 0 0 255)
 (color 0 0 0 255)
 (color 0 0 0 255)
 (color 0 0 0 255)
 (color 0 0 0 255)
 (color 0 0 0 255)
 (color 0 0 0 253)
 (color 255 255 255 0)
 (color 255 255 255 0)
 (color 255 255 255 0)
 (color 255 255 255 0)
 (color 255 255 255 0)
 (color 255 255 255 0)
 (color 255 255 255 0)
 (color 255 255 255 0)
 (color 255 255 255 0)
 (color 255 255 255 0)))