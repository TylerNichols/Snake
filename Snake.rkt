#lang racket/base
(require 2htdp/universe)
(require 2htdp/image)

;; Here is the source code for the game Snake written in Racket
;; This is a good introduction to basic Racket/Scheme code and also
;; shows the power and ease of creating a simple game in a very fun and
;; productive language. The source will be overly commented as this
;; is going to be targetted to people of all levels. If there is any
;; syntax that does not make sense, I highly suggest you consult the
;; Racket documentation, as it is the best documentation ever! This is
;; reminiscent of a project you could see in a intro level university
;; course that focuses on the Racket language.

;; What is Snake?
;; Snake is a game that everyone should be familiar with. There are
;; different versions of snake, from the classical grid based versions
;; to the newer versions such as slither.io with non grid movement. We
;; will focus on the grid version for this tutorial.

;; The first thing we want to do is define some constants.
;; You may wonder -- if these things are constant, why
;; would we define a variable? Well one reason is to reduce
;; errors when we want to alter the size the board next time
;; we run the file. Another reason is because just leaving a
;; number floating around in your code (known as a
;; "magic number") will make it hard to read and understand
;; what the number actually represents.

;; Constants that define size of the board
;; Notice here, we are using 'define' to give a variable a constant value.
;; For this game we will have only one board. Feel free to mess around with
;; the size of the board, the game will be written to be board size agnostic.
(define board-width 30)
(define board-height 30)
(define tile-size 15)

(define background (empty-scene
                    (* board-width tile-size)
                    (* board-height tile-size)))

;; We will also define how many parts a food adds to a snake
(define food-value 3)

;; After we define our constants, we want to think of the minimal ammount
;; of data we need to simulate the game. Since this is a snake game, we need
;; to focus on a few things. We already have board dimensions taken care of,
;; so next we should focus on the snake itself. That begs the question: what
;; are the parts of the snake?
;; A snake has:
;;   Body parts (a head and tail parts)
;;   A direction that it is moving
;; That's really it. So, we need to figure out how to best represent those parts.

;; The first thing we can focus on are the body parts;
;; Since we are doing a grid version of the game, each part can be
;; simply represented as an x and y value.

;; [part]
;; x -> the x location of the part
;; y -> the y location of the part
(define-struct part (x y))
(define apart (make-part 15 15)) ;; Defining a structure with values

;; The next thing we need is a direction.
;; A direction can be represented multiple ways, but for simplicity we will use
;; a string. For clarity sake, I will explicitly define the definition here

;; A direction is either:
;;   - "up"
;;   - "down"
;;   - "left"
;;   - "right"

;; Now we have everything needed to define what a snake is!

;; [snake]
;; parts -> A list of part
;; direction -> a direction
(define-struct snake (parts direction))
(define asnake (make-snake (list apart) "up"))

;; We also need the food for the snake.
;; Like a part, it also is just an x and y location on our grid.
;; You could get creative and have the food have other fields,
;; such as value, etc, to grow the snake by variable ammounts.

;; [food]
;; x -> the x location of the food
;; y -> the y location of the food
(define-struct food (x y))
(define afood (make-food 20 20))

;; [snake-world]
;; snake -> the snake
;; foods -> the list of food 
(define-struct snake-world (snake foods))
(define aworld (make-snake-world asnake (list afood)))

;; It's as easy as that. We have defined all necessary structures
;; and constants for our snake game.

;; Alright! now it's time to start making things draw.



;; render-part: [part] [image] -> [image]
(define (render-part part background)
  (place-image
   (square tile-size "solid" "slateblue")
   (* tile-size (part-x part))
   (* tile-size (part-y part))
   background))

;; render-parts: [list of parts] [image] -> [image]
(define (render-parts lop background)
  (foldr render-part background lop))

;; render-snake: [snake-world] [image] -> [image]
;; I prefer to make my render methods take the world
;; and then take out the specific part. I like the way
;; they compose better. Feel free to alter that if
;; you wish
(define (render-snake-world sw background)
  (render-parts
   (snake-parts (snake-world-snake sw))
   background))

;; render-food: [food] [image] -> [image]
(define (render-food food background )
  (place-image
   (square tile-size "solid" "red")
   (* tile-size (food-x food))
   (* tile-size (food-y food))
   background))

;; render-foods: [list of foods] [image] -> image
(define (render-foods lof background)
  (foldr render-food background lof))

;; render-foods-world: [snake-world] [image] -> [image]
(define (render-foods-world sw background)
  (render-foods (snake-world-foods sw) background))
   
;; render-world: [snake-world] -> [image]
(define (render-world sw)
  (render-foods-world sw
                      (render-snake-world sw background)))

;; change-snake-direction [snake] [direction] -> [snake]
(define (change-snake-direction asnake direction)
  (make-snake (snake-parts asnake) direction))

;; change-snake-direction-world: [snake-world] [direction] -> [snake-world]
(define (change-snake-direction-world sw direction)
  (make-snake-world
   (change-snake-direction (snake-world-snake sw) direction)
   (snake-world-foods sw)))

;; handle-key: [snake-world] [key] -> [snake-world]
(define (handle-key sw akey)
  (cond
    [(or (key=? akey "up")
         (key=? akey "down")
         (key=? akey "left")
         (key=? akey "right")) (change-snake-direction-world sw akey)]
    [else sw]))

;; remove-last-part: [list-of-parts] -> [list-of-parts]
(define (remove-last-part lop)
  (cond [(null? lop) '()]
        [(null? (cdr lop)) '()]
        [else (cons (car lop)
                    (remove-last-part (cdr lop)))]))
  

;; add-head-part-dxdy: [list-of-parts] [integer] [integer] -> [list-of-parts]
(define (add-head-part-dxdy lop dx dy)
  (cons (make-part
         (+ (part-x (car lop)) dx)
         (+ (part-y (car lop)) dy)) lop))

;; add-head-part: [list-of-parts] [direction] -> [list-of-parts]
(define (add-head-part lop direction)
  (cond
    [(string=? direction "up") (add-head-part-dxdy lop 0 -1)]
    [(string=? direction "down") (add-head-part-dxdy lop 0 1)]
    [(string=? direction "left") (add-head-part-dxdy lop -1 0)]
    [(string=? direction "right") (add-head-part-dxdy lop 1 0)]))

;; move-snake [snake] -> [snake]
(define (move-snake snake)
  (make-snake
   (remove-last-part
    (add-head-part (snake-parts snake) (snake-direction snake))) ;; order matters!
   (snake-direction snake)));)

;; move-snake-world: [snake-world] -> [snake-world]
(define (move-snake-world sw)
  (make-snake-world
   (move-snake (snake-world-snake sw))
   (snake-world-foods sw)))


;; handle-tick: [snake-world] -> [snake-world]
(define (handle-tick sw)
  (move-snake-world sw));;(move-snake-world sw))


;; big-bang main fn
(define (main sw)
  (big-bang sw
            (on-tick handle-tick)
            (to-draw render-world)
            (on-key handle-key)))


(main aworld)

