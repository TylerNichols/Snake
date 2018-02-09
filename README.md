# Snake
A snake game written in Racket with readability for beginners in mind


(/images/game.PNG)


Some musings are contained along with the code that will later be combined to create a simple tutorial. 




# TODO
* Fix bug where it is possible to move twice in one tick causing snake to hit itself
   (Can use some global lock or just store it in the snake like the snake part buffer)
* Abstract out x and y from part/food and use a posn struct in its place
