;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname BOGOSORT) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; To use, type (run [List-Of Numbers]) in the console
; When the sort is complete, the amount of time it took to sort
; the list appears on screen.

(require 2htdp/universe)
(require 2htdp/image)

(define SCENE-WIDTH 800)
(define SCENE-HEIGHT 400)
(define BACKGROUND (rectangle SCENE-WIDTH SCENE-HEIGHT "solid" "black"))

; A BogoWorld is a (make-bogo ListOfNumbers Number)
(define-struct bogo [num-list timer])

(define WORLD0 (list 1 2 3 5 7 9 11 6))

(define TICKRATE 26)


; randomize : List Number -> List
; Moves a random number to the front of the list "n" times
(define (randomize a-list n)
  (local
    [(define num (random (length a-list)))
     (define selected (list-ref a-list num))]
    (cond
      [(zero? n) a-list]
      [else (randomize (append (cons selected '())
                               (get-left a-list num)
                               (get-right a-list num))
                       (sub1 n))]
      )
    )
  )
; factorial : Number -> Number
; Calculates the factorial of a number
(define (factorial x)
  (if (> x 0)
      (* x (factorial (sub1 x)))
      1)
  )

; estimated-time : List -> Number
; Estimates the time of how long the bogosort will take
(define (estimated-time a-list)
  (sec->time (/ (factorial (length a-list)) TICKRATE))
  )
; sec->time : Number -> String
; Converts a time in seconds to hr:min:s
(define (sec->time num)
  (string-append (number->string (floor (/ (round num) 3600))) ":"
                 (number->string (floor (/ (modulo (round num) 3600) 60))) ":"
                 (number->string (floor (modulo (modulo (round num) 3600) 60))))
  )

; get-right: List Number -> Number
; gets all of the items to the right of the index
(define (get-right a-list n)
  (cond
    [(zero? n) (rest a-list)]
    [else (get-right (rest a-list) (sub1 n))]
    )
  )
; get-left : List Number -> Number
; gets all of the items to the left of the index
(define (get-left a-list n)
  (cond
    [(zero? n) '()]
    [else (cons (first a-list) (get-left (rest a-list) (sub1 n)))
          ]))
  
; rand-sort: BogoWorld -> BogoWorld
; if the list isn't sorted, scramble it
(define (rand-sort world)
  (if (sorted? (bogo-num-list world))
      
      world
      
      (make-bogo (randomize (bogo-num-list world)
                            (sqr (length (bogo-num-list world))))
                 
                 (add1 (bogo-timer world)))
      )
  )

; render-scene : BogoWorld -> Image
; Renders the scene
(define (render-scene world)
  (if (sorted? (bogo-num-list world))
      
      (place-image
       (text (sec->time (/ (bogo-timer world) TICKRATE)) 20 "red")
       (/ SCENE-WIDTH 2)  40
       (render-rectangles (bogo-num-list world)
                          (/ SCENE-WIDTH (length (bogo-num-list world)))
                          (/ SCENE-HEIGHT (foldl max
                                                 (first (bogo-num-list world))
                                                 (rest (bogo-num-list world))))
                          BACKGROUND))
  
      (render-rectangles (bogo-num-list world)
                         (/ SCENE-WIDTH (length (bogo-num-list world)))
                         (/ SCENE-HEIGHT (foldl max
                                                (first (bogo-num-list world))
                                                (rest (bogo-num-list world))))
                         BACKGROUND))
  )

; render-rectanbles : List Number Number Image
; Renders rectangles sized proportionally to the size of the number
(define (render-rectangles a-list w h background)
 (if (< 2 (length a-list))
  (cond
    [(empty? a-list) background]
    [(sorted? (list (first a-list) (second a-list)))
     (place-image (rectangle w (* h 2 (first a-list)) "solid" "green")
                  (+ (/ w 2) (- SCENE-WIDTH (* w (length a-list))))
                  SCENE-HEIGHT (render-rectangles
                                (rest a-list) w h background))]
    [else
     (place-image (rectangle w (* h 2 (first a-list)) "solid" "white")
                  (+ (/ w 2) (- SCENE-WIDTH (* w (length a-list))))
                  SCENE-HEIGHT (render-rectangles
                                (rest a-list) w h background))])
   (cond
    [(empty? a-list) background]
    [(sorted? a-list)
     (place-image (rectangle w (* h 2 (first a-list)) "solid" "green")
                  (+ (/ w 2) (- SCENE-WIDTH (* w (length a-list))))
                  SCENE-HEIGHT (render-rectangles
                                (rest a-list) w h background))]
    [else
     (place-image (rectangle w (* h 2 (first a-list)) "solid" "white")
                  (+ (/ w 2) (- SCENE-WIDTH (* w (length a-list))))
                  SCENE-HEIGHT (render-rectangles
                                (rest a-list) w h background))])
  ))


; sorted? : List -> Boolean
; Determines if the list is sorted
(define (sorted? a-list)
  (equal? (quicksort a-list <) a-list)
  )

(define (run world)
  (big-bang (make-bogo world 0)
    [to-draw render-scene] 
    [on-tick rand-sort (/ 1 TICKRATE)]
          
    )
  )