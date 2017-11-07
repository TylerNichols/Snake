#lang racket/base
(require 2htdp/universe)
(require 2htdp/image)
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
(define apart (make-part 15 15))

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

;; handle-tick: [snake-world] -> [snake-world]
(define (handle-tick sw)
  sw)

;; render-part: [part] [image] -> [image]
(define (render-part part background )
  (place-image
   (square tile-size "solid" "slateblue")
   (* tile-size (part-x part))
   (* tile-size (part-y part))
   background))

;; render-parts: [list of parts] [image] -> [image]
(define (render-parts lop background)
  (foldr render-part background lop))

;; render-snake: [snake-world] [image] -> [image]
(define (render-snake sw background)
  (render-parts
   (snake-parts (snake-world-snake sw))
   background))
  
;; render-world: [snake-world] -> [image]
(define (render-world sw)
  (render-snake sw background))

;; big-bang main fn
(define (main sw)
  (big-bang sw
            (on-tick handle-tick)
            (to-draw render-world)))


(main aworld)

