#lang racket
; gui.rkt
(require
 racket/gui
 racket/draw)

; Create a new GUI.
(define gui%
  (class object%
    (init-field width
                height)

    ; Create the frame.
    (define frame
      (new frame%
           [label "Racket Survival"]
           [style '(no-resize-border)]))

    ; Create the canvas
    (define canvas
      (new canvas%
           [parent frame]
           [min-width width]
           [min-height height]
           [stretchable-width width]
           [stretchable-height height]))

    ; Create a bitmap on the canvas
    (define dc (send canvas get-dc))
    
    ; Make everything visible
    (send frame show #t)
    
    ; Finish initilization.
    (super-new)))

(define gui (new gui%
     [width 400]
     [height 240]))

    (send (gui dc) draw-line
      0 0    ; Start at (0, 0), the top-left corner
      30 30) ; and draw to (30, 30), the bottom-right corner
    
