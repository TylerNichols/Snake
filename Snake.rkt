#lang racket/base
(require 2htdp/universe)
(require 2htdp/image)

;; Here is the source code for the game Snake written in Racket
;; This is a good introduction to basic Racket/Scheme code and also
;; shows the power and ease of creating a simple game in a very fun and
;; productive language.  If there is any syntax that does not make sense,
;; I highly suggest you consult the  Racket documentation,
;; as it is the best documentation ever! This is reminiscent
;; of a project you could see in a intro level university course
;; that focuses on the Racket language.

;; What is Snake?
;; Snake is a game that everyone should be familiar with. There are
;; different versions of snake, from the classical grid based versions
;; to the newer versions such as slither.io with non grid movement. We
;; will focus on the grid version for this tutorial.

;; If you want to see how it all comes together, look for the
;; "big-bang" function at the bottom of the project. Again,
;; If anything looks strange,  take a look at the Racket documentation
;; at https://docs.racket-lang.org/

;;==============================================================================
;;   ____                    _                 _        
;;  / ___| ___   _ __   ___ | |_  __ _  _ __  | |_  ___ 
;; | |    / _ \ | '_ \ / __|| __|/ _` || '_ \ | __|/ __|
;; | |___| (_) || | | |\__ \| |_| (_| || | | || |_ \__ \
;;  \____|\___/ |_| |_||___/ \__|\__,_||_| |_| \__||___/
;;
;;==============================================================================
;; The first thing we want to do is define some constants.
;; You may wonder -- if these things are constant, why
;; would we define a variable? Well one reason is to reduce
;; errors when we want to alter the size the board next time
;; we run the file. Another reason is because just leaving a
;; number floating around in your code (known as a
;; "magic number") will reduce maintainability and readability
;; of the source code


;; Constants that define size of the board
;; For this game we will have only one board. Feel free to mess around with
;; the size of the board, the game will be written to be board size agnostic.
(define board-width 64)
(define board-height 64)
(define tile-size 12)

(define background-scene (empty-scene
                          (+ (* board-width tile-size) (/ tile-size 2))
                          (+ (* board-height tile-size) (/ tile-size 2))))

(define vertical-wall (rectangle
                       (/ tile-size 2)
                       (* (+ 1 board-height) tile-size)
                       "solid" "black"))

(define horizontal-wall (rectangle
                         (* (+ 1 board-width) tile-size)
                         (/ tile-size 2)
                         "solid" "black"))

(define background-with-walls
  (overlay/xy
   horizontal-wall 0 (- (* (+ .5 board-height) tile-size))
   (overlay/xy
    horizontal-wall 0 0
    (overlay/xy
     vertical-wall 0 0
     (overlay/xy
      vertical-wall (- (* (+ .5 board-width) tile-size)) 0 background-scene)))))

(define background background-with-walls)

;; We will also define how many parts a food adds to a snake and
;; a spawn chance we will use later to randomly spawn food
(define food-value 3)
(define food-spawn-chance 50)

;;==============================================================================
;;  ____   _                       _                          
;; / ___| | |_  _ __  _   _   ___ | |_  _   _  _ __  ___  ___ 
;; \___ \ | __|| '__|| | | | / __|| __|| | | || '__|/ _ \/ __|
;;  ___) || |_ | |   | |_| || (__ | |_ | |_| || |  |  __/\__ \
;; |____/  \__||_|    \__,_| \___| \__| \__,_||_|   \___||___/
;;
;;==============================================================================
;; After we define our constants, we want to think of the minimal ammount
;; of data we need to simulate the game. Since this is a snake game, we need
;; to focus on a few things. We already have board dimensions taken care of,
;; so next we should focus on the snake itself. That begs the question: what
;; are the parts of the snake?
;; A snake has:
;;   Body parts (a head and tail parts)
;;   A direction that it is moving
;;   A buffer (how many parts we have to add to the snake)
;;     when a snake eats a food, it grows an extra part whenever it moves
;;     we need to keep track of how many parts we have to add
;; That's really it.
;; So, we need to figure out how to best represent those parts.

;; The first thing we can focus on are the body parts;
;; Since we are doing a grid version of the game, each part can be
;; simply represented as an x and y value.

;; [part]
;; x -> the x location of the part
;; y -> the y location of the part
(define-struct part (x y))
(define apart1 (make-part 32 32)) ;; Defining a structure with values
(define apart2 (make-part 32 33))
(define apart3 (make-part 32 34))
(define apart4 (make-part 32 35))
(define apart5 (make-part 32 36))
(define apart6 (make-part 32 37))

;; The next thing we need is a direction.
;; A direction can be represented multiple ways, but for simplicity we will use
;; a string. For clarity sake, I will explicitly enumerate the definition here

;; A direction is either:
;;   - "up"
;;   - "down"
;;   - "left"
;;   - "right"

;; Now we have everything needed to define what a snake is!

;; [snake]
;; parts -> A list of part
;; direction -> a direction
;; buffer -> number of parts left to add to the snake (more on this later)
(define-struct snake (parts direction buffer))
(define asnake
  (make-snake
   (list apart1 apart2 apart3 apart4 apart5 apart6)
   "up"
   0))

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

;;==============================================================================
;;  ____                         _               
;; |  _ \  _ __  __ _ __      __(_) _ __    __ _ 
;; | | | || '__|/ _` |\ \ /\ / /| || '_ \  / _` |
;; | |_| || |  | (_| | \ V  V / | || | | || (_| |
;; |____/ |_|   \__,_|  \_/\_/  |_||_| |_| \__, |
;;                                         |___/
;;==============================================================================
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
  (render-foods-world sw (render-snake-world sw background)))

;;==============================================================================
;;  ___                       _   
;; |_ _| _ __   _ __   _   _ | |_ 
;;  | | | '_ \ | '_ \ | | | || __|
;;  | | | | | || |_) || |_| || |_ 
;; |___||_| |_|| .__/  \__,_| \__|
;;             |_|  
;;==============================================================================

;; change-snake-direction [snake] [direction] -> [snake]
(define (change-snake-direction asnake direction)
  (make-snake (snake-parts asnake) direction (snake-buffer asnake)))

;; change-snake-direction-world: [snake-world] [direction] -> [snake-world]
(define (change-snake-direction-world sw direction)
  (make-snake-world
   (change-snake-direction (snake-world-snake sw) direction)
   (snake-world-foods sw)))

;; handle-key: [snake-world] [key] -> [snake-world]
(define (handle-key sw akey)
  (cond
    [(or (and (key=? akey "up")
              (not (string=? (snake-direction (snake-world-snake sw)) "down"))) 
         (and (key=? akey "down")
              (not (string=? (snake-direction (snake-world-snake sw)) "up"))) 
         (and (key=? akey "left")
              (not (string=? (snake-direction (snake-world-snake sw)) "right"))) 
         (and (key=? akey "right")
              (not (string=? (snake-direction (snake-world-snake sw)) "left"))))
     (change-snake-direction-world sw akey)]
    [else sw]))

;;==============================================================================
;;  _____  _        _    
;; |_   _|(_)  ___ | | __
;;   | |  | | / __|| |/ /
;;   | |  | || (__ |   < 
;;   |_|  |_| \___||_|\_\
;;
;;==============================================================================
                 
;; remove-last-part: [list-of-parts] -> [list-of-parts]
(define (remove-last-part lop)
  (cond [(null? lop) '()] ;; should never happen... :)
        [(null? (cdr lop)) '()] ;; if we have buffered parts, don't remove part
        [else (cons (car lop)
                    (remove-last-part (cdr lop)))]))

;; remove-snake-tail
(define (remove-snake-tail snake)
  (if (> (snake-buffer snake) 0)
      snake
      (make-snake
       (remove-last-part (snake-parts snake))
       (snake-direction snake)
       (snake-buffer snake))))

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

;; move-snake: [snake] -> [snake]
(define (move-snake-head snake)
  (make-snake
   (add-head-part
    (snake-parts snake)
    (snake-direction snake)) ;; order matters!
   (snake-direction snake)
   (snake-buffer snake)))

;; move-snake-world: [snake-world] -> [snake-world]
(define (move-snake-world sw)
  (make-snake-world
   (remove-snake-tail (move-snake-head (snake-world-snake sw)))
   (snake-world-foods sw)))

;; add-parts: [snake] -> [snake]
(define (add-parts snake)
  (make-snake
   (snake-parts snake)
   (snake-direction snake)
   (+ food-value (snake-buffer snake))))

;; collides? [part] [food] -> [boolean]
(define (collides? part food)
  (and
   (= (part-x part) (food-x food))
   (= (part-y part) (food-y food))))

;; any-collides?: [part] [list-of-food] -> [boolean]
(define (snake-head-collides? snake-head foods)
  (ormap
   (lambda (x) (collides? snake-head x))
   foods))

;; grow-if-collides: [snake] [foods] -> [snake]
(define (grow-if-collides snake foods)
  (if
   (snake-head-collides? (car (snake-parts snake)) foods)
   (add-parts snake)
   snake))

;; grow-snake-world: [snake-world] -> [snake-world]
(define (grow-snake-world sw)
  (make-snake-world
   (grow-if-collides (snake-world-snake sw) (snake-world-foods sw))
   (snake-world-foods sw)))

;; reduce-snake-buffer: [snake] -> [snake]
(define (reduce-snake-buffer snake)
  (make-snake
   (snake-parts snake)
   (snake-direction snake)
   (if (> (snake-buffer snake) 0)
       (- (snake-buffer snake) 1)
       (snake-buffer snake))))

;; reduce-snake-buffer-world: [snake-world] -> [snake-world]
(define (reduce-snake-buffer-world sw)
  (make-snake-world
   (reduce-snake-buffer (snake-world-snake sw))
   (snake-world-foods sw)))

;; destroy-food: [snake-world] -> [list-of-food]
(define (destroy-food sw)
  (filter
   (lambda (x) (not
                (collides?
                 (car (snake-parts (snake-world-snake sw)))
                 x)))
   (snake-world-foods sw)))

;; destroy-food-world: [snake-world] -> [snake-world]
(define (destroy-food-world sw)
  (make-snake-world
   (snake-world-snake sw)
   (destroy-food sw)))

;; spawn-random-food: [] -> [food]
(define (spawn-random-food)
  (make-food (+ 2 (random (- board-width 4)))
             (+ 2 (random (- board-height 4)))))
              
;; spawn-food: [snake-world] -> [list-of-food]
(define (spawn-food sw)
  (if (or (null? (snake-world-foods sw))
          (= (random food-spawn-chance) 1))
      (cons (spawn-random-food) (snake-world-foods sw))
      (snake-world-foods sw)))

;; spawn-food-world: [snake-world] -> [snake-world]
(define (spawn-food-world sw)
  (make-snake-world
   (snake-world-snake sw)
   (spawn-food sw)))
  
;; handle-tick: [snake-world] -> [snake-world]
(define (handle-tick sw)
  (spawn-food-world
   (destroy-food-world
    (reduce-snake-buffer-world
     (grow-snake-world
      (move-snake-world sw))))))



;;==============================================================================
;;   ____                            ___                    
;;  / ___|  __ _  _ __ ___    ___   / _ \ __   __ ___  _ __ 
;; | |  _  / _` || '_ ` _ \  / _ \ | | | |\ \ / // _ \| '__|
;; | |_| || (_| || | | | | ||  __/ | |_| | \ V /|  __/| |   
;;  \____| \__,_||_| |_| |_| \___|  \___/   \_/  \___||_|   
;;                                                          
;;==============================================================================


;; snake-collision-wall? [snake-world] -> [boolean]
(define (snake-collision-wall? sw)
  (or (not (< 0
              (part-x (car (snake-parts (snake-world-snake sw))))
              (+ 1 board-width)))
      (not (< 0
              (part-y (car (snake-parts (snake-world-snake sw))))
              (+ 1 board-height)))))

;; any-collides? [part] [list-of-parts] -> [boolean]
(define (any-collides? head lop)
  (ormap
   (lambda (body)
     (and (= (part-x head) (part-x body))
          (= (part-y head) (part-y body))))
   lop))
                
;; snake-collision-snake? {snake-world] -> [boolean]
(define (snake-collision-snake? sw)
  (any-collides?
   (car (snake-parts (snake-world-snake sw)))
   (cdr (snake-parts (snake-world-snake sw)))))

;; handle-game-over: [snake-world] -> [boolean]
(define (handle-game-over sw)
  (or (snake-collision-wall? sw)
      (snake-collision-snake? sw)))

;;==============================================================================
;;   ____                           _                         
;;  / ___|  __ _  _ __ ___    ___  | |     ___    ___   _ __  
;; | |  _  / _` || '_ ` _ \  / _ \ | |    / _ \  / _ \ | '_ \ 
;; | |_| || (_| || | | | | ||  __/ | |___| (_) || (_) || |_) |
;;  \____| \__,_||_| |_| |_| \___| |_____|\___/  \___/ | .__/ 
;;                                                     |_| 
;;==============================================================================

;; big-bang main fn
(define (main sw)
  (big-bang sw
            (on-tick handle-tick) ;; update game
            (to-draw render-world) ;; draw game
            (on-key handle-key) ;; handle key presses
            (stop-when handle-game-over))) ;; handle game over states

(main aworld) ;; run the game