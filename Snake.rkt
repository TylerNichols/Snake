#lang racket/base
;; Here is the source code for the game Snake written in Racket
;; This is a good introduction to basic Racket/Scheme code and also
;; shows the power and ease of creating a simple game in a very fun and
;; productive language. The source will be overly commented as this
;; is going to be targetted to people of all levels.


;; The first thing we want to do is define some constants.
;; You may wonder -- if these things are constant, why
;; would we define a variable?

;; Constants that define size of the board
;; Notice here, we are using 'define' to give a variable
;; a constant value
(define board-width 30)
(define board-height 30)

;; After we define our constants, we want to think of the minimal ammount
;; of data we need to simulate the game. 

;; Defining structures that will be used in the game.
;; All of our game data will be held in structures
(define-struct part (posn))
(define-struct snake (parts direction))
(define-struct food (posn))

