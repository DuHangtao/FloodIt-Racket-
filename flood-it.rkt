;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname flood-it) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; --------------------Data definitions & templates--------------------

;; A World is one of:
;; - StillWorld
;; - FloodingWorld

;; A StillWorld is a (make-still [List-of Tile] Natural)
;; and represents a world that can be clicked with a list
;; of tiles and the # of clicks taken so far
(define-struct still [tiles clicks])


;; A FloodingWorld is (make-flooding [List-of Tile] [List-of Tile] Natural Color Color)
;; and is a world in the process of being flooded with a list of tiles
;; that have been flooded, tiles that might be flooded, the # of clicks taken
;; so far, the color that is being flooded from, and the color that is being flooded to
(define-struct flooding [flooded unflooded clicks flooding-from flooding-to])

;; world-temp : World -> ?
#;(define (world-temp w)
    (... (cond [(still? w) (still-temp w)]
               [(flooding? w) (flooding-temp w)])))

;; still-temp : StillWorld -> ?
#;(define (still-temp sw)
    (... (still-tiles sw) ... (still-clicks sw) ...))

;; flooding-temp : FloodingWorld -> ?
#;(define (flooding-temp fw)
    (... (flooding-flooded fw) ... (flooding-unflooded fw) ... (flooding-clicks fw) ...
         (flooding-flooding-from fw) ... (flooding-flooding-to fw) ...))

;; A Tile is a (make-tile Posn Color)
;; and represents the grid position of a tile and its color
(define-struct tile [posn color])

;; tile-temp : Tile -> ?
#;(define (tile-temp t)
    (... (tile-posn t) ... (tile-color t) ...))

;; A Color is a Symbol
(define color=? symbol=?)


;; --------------------Data examples--------------------

(define STILL
  (make-still
   (list (make-tile (make-posn 0 0) 'red)
         (make-tile (make-posn 0 1) 'red)
         (make-tile (make-posn 0 2) 'red)
         (make-tile (make-posn 1 0) 'red)
         (make-tile (make-posn 1 1) 'blue)
         (make-tile (make-posn 1 2) 'green)
         (make-tile (make-posn 2 0) 'green)
         (make-tile (make-posn 2 1) 'orange)
         (make-tile (make-posn 2 2) 'blue))
   0))

;; A flooding progression
(define FLOODING
  (make-flooding
   (list (make-tile (make-posn 0 0) 'green))
   (list (make-tile (make-posn 0 1) 'red)
         (make-tile (make-posn 0 2) 'red)
         (make-tile (make-posn 1 0) 'red)
         (make-tile (make-posn 1 1) 'blue)
         (make-tile (make-posn 1 2) 'green)
         (make-tile (make-posn 2 0) 'green)
         (make-tile (make-posn 2 1) 'orange)
         (make-tile (make-posn 2 2) 'blue))
   1
   'red
   'green))
(define FLOODING-1
  (make-flooding
   (list (make-tile (make-posn 0 0) 'green)
         (make-tile (make-posn 0 1) 'green)
         (make-tile (make-posn 1 0) 'green))
   (list
    (make-tile (make-posn 0 2) 'red)
    (make-tile (make-posn 1 1) 'blue)
    (make-tile (make-posn 1 2) 'green)
    (make-tile (make-posn 2 0) 'green)
    (make-tile (make-posn 2 1) 'orange)
    (make-tile (make-posn 2 2) 'blue))
   1
   'red
   'green))
(define FLOODING-2
  (make-flooding
   (list
    (make-tile (make-posn 0 0) 'green)
    (make-tile (make-posn 0 1) 'green)
    (make-tile (make-posn 1 0) 'green)
    (make-tile (make-posn 0 2) 'green))
   (list
    (make-tile (make-posn 1 1) 'blue)
    (make-tile (make-posn 1 2) 'green)
    (make-tile (make-posn 2 0) 'green)
    (make-tile (make-posn 2 1) 'orange)
    (make-tile (make-posn 2 2) 'blue))
   1
   'red
   'green))
(define BACK-TO-STILL
  (make-still
   (list
    (make-tile (make-posn 0 0) 'green)
    (make-tile (make-posn 0 1) 'green)
    (make-tile (make-posn 1 0) 'green)
    (make-tile (make-posn 0 2) 'green)
    (make-tile (make-posn 1 1) 'blue)
    (make-tile (make-posn 1 2) 'green)
    (make-tile (make-posn 2 0) 'green)
    (make-tile (make-posn 2 1) 'orange)
    (make-tile (make-posn 2 2) 'blue))
   1))

(define UPPER-LEFT-RED (make-tile (make-posn 0 0) 'red))


;;--------------------Data utility functions--------------------

;; world-tiles : World -> [List-of Tile]
;; Grab the tiles of the world
(define (world-tiles w)
  (cond [(still? w) (still-tiles w)]
        [(flooding? w) (append (flooding-flooded w) (flooding-unflooded w))]))
(check-expect (world-tiles STILL) (still-tiles STILL))
(check-expect (world-tiles FLOODING) (cons (tyle 0 0 'green) (flooding-unflooded FLOODING)))

;; world-clicks : World -> Natural
;; Grab the clicks of the world
(define (world-clicks w)
  (cond [(still? w) (still-clicks w)]
        [(flooding? w) (flooding-clicks w)]))
(check-expect (world-clicks STILL) 0)
(check-expect (world-clicks FLOODING) 1)

;; tyle : Natural Natural Color -> Tile
;; A convenient constructor for tiles
(define (tyle x y c)
  (make-tile (make-posn x y) c))
(check-expect (tyle 0 0 'red) UPPER-LEFT-RED)

;; update-color : Tile Color -> Tile
;; Update the color of a tile
(define (update-color t c)
  (make-tile (tile-posn t) c))
(check-expect (update-color UPPER-LEFT-RED 'green)
              (tyle 0 0 'green))


;; --------------------Graphical constants--------------------

(define CELL-SIZE 20)
(define COLORS '(green purple orange red blue yellow pink white black))

;;--------------------main--------------------

;; main : PosInt [1, 9] -> Natural
;; Given the grid size and amount of colors to play with, launch the game
(define (main grid-size colors)
  (world-clicks
   (big-bang (still-world grid-size colors)
             [to-draw (λ (w) (draw grid-size w))]
             [on-tick flood]
             [on-mouse click]
             [stop-when all-equal?]))) 


;; still-world : PosInt [1, 9] -> StillWorld
;; Given the grid size and amount of colors to play with, make the initial world
(define (still-world grid-size colors)
  (local ((define (column x)
            (build-list grid-size (λ (y) (tyle x y (list-ref COLORS (random colors)))))))
    (make-still (foldr append '() (build-list grid-size column)) 0)))
(check-expect (world-clicks (still-world 3 4)) 0)
(check-expect (length (world-tiles (still-world 3 4))) 9)
(check-expect (length (filter (λ (tile) (= 0 (posn-x (tile-posn tile))))
                              (world-tiles (still-world 3 4))))
              3)
(check-expect (length (filter (λ (tile) (= 0 (posn-y (tile-posn tile))))
                              (world-tiles (still-world 3 4))))
              3)
(check-expect (andmap (λ (tile) (color=? (first COLORS) (tile-color tile)))
                      (world-tiles (still-world 3 1)))
              #t)


;;--------------------flood--------------------
;(define-struct flooding [flooded unflooded clicks flooding-from flooding-to])

;; flood : World -> World
;; Advance the game if it is in a flooding state
(define (flood w)
  (cond [(still? w) w]
        [(flooding? w) (flooding-flood w)]))
(check-expect (flood STILL) STILL)
(check-expect (flood FLOODING) (flooding-flood FLOODING))

;; flooding-flood : FloodingWorld -> World
;; Flood a flooding world one more layer if there is more to flood,
;; otherwise, return back to a still world
(check-expect (flooding-flood FLOODING)   FLOODING-1)
(check-expect (flooding-flood FLOODING-1) FLOODING-2)
(check-expect (flooding-flood FLOODING-2) BACK-TO-STILL)

(define (flooding-flood fw)
  (local ((define from (flooding-flooding-from fw))
          (define to (flooding-flooding-to fw))
          (define (filter-tiles fw-flooded fw-unflooded)
            (local ((define posn tile-posn))      
              (filter (lambda (x) 
                        (ormap (lambda(y)(or (and (= (- (posn-x (posn x)) 1) (posn-x (posn y)))
                                                  (= (posn-y (posn x)) (posn-y (posn y))))
                                             (and (= (+ (posn-y (posn x)) 1) (posn-y (posn y)))
                                                  (= (posn-x (posn x)) (posn-x (posn y))))
                                             (and (= (+ (posn-x (posn x)) 1) (posn-x (posn y)))
                                                  (= (posn-y (posn x)) (posn-y (posn y))))
                                             (and (= (- (posn-y (posn x)) 1) (posn-y (posn y)))
                                                  (= (posn-x (posn x)) (posn-x (posn y))))))
                               fw-flooded)) fw-unflooded)))
          (define neighbors (filter-tiles (flooding-flooded fw)(flooding-unflooded fw))))
    (if
     (ormap (lambda(x) (color=? from (tile-color x))) neighbors)
     (local
       ((define same-color-tile (filter(lambda (x)(color=? from (tile-color x))) neighbors))
        (define (removing a b)
          (filter (lambda (x)
                    (andmap (lambda (y)(not (posn=? (tile-posn x)(tile-posn y)))) a)) b)))
       (make-flooding
        (append (flooding-flooded fw)
                (map (lambda (x)(update-color x to)) same-color-tile))                            
        (removing same-color-tile (flooding-unflooded fw))
        (flooding-clicks fw) 
        from
        to))
     (make-still (append (flooding-flooded fw) (flooding-unflooded fw))
                 (flooding-clicks fw)))))     
     
;;--------------------draw--------------------

;; draw : Natural World -> Image
;; Draw the world
(define (draw gride-size w)
  (local ((define (place position)
            (* (+ .5 position) CELL-SIZE)))
    (foldr (λ (t i) (place-image (square CELL-SIZE 'solid (tile-color t))
                                 (place (posn-x (tile-posn t)))
                                 (place (posn-y (tile-posn t)))
                                 i))
           (empty-scene (* gride-size CELL-SIZE) (* gride-size CELL-SIZE))
           (world-tiles w))))
(check-expect (draw 2 (make-still (list (tyle 0 0 'red)
                                        (tyle 1 0 'blue)
                                        (tyle 0 1 'green)
                                        (tyle 1 1 'orange))
                                  0))
              (above (beside (square CELL-SIZE 'solid 'red)   (square CELL-SIZE 'solid 'blue))
                     (beside (square CELL-SIZE 'solid 'green) (square CELL-SIZE 'solid 'orange))))

;;--------------------click--------------------

;; click : World Natural Natural ME  -> World
;; Let the flooding begin (if appropriate)
(define (click w x y me)
  (cond
    [(or (flooding? w) (not (string=? me "button-down"))) w]
    [else (local ((define flooding-to (tile-color (tile-selection x y w)))
                  (define flooding-from (tile-color (upper-left-tile (world-tiles w)))))
            (cond [(color=? flooding-to flooding-from) w]
                  [else (still->flooding w flooding-to)]))]))
(check-expect (click FLOODING 0 0 "button-down") FLOODING)
(check-expect (click STILL 0 0 "button-up") STILL)
(check-expect (click STILL 0 0 "button-down") STILL)
(check-expect (click STILL 41 0 "button-down") FLOODING)

;; still->flooding : StillWorld Color -> FloodingWorld
;; Begin the flooding process after a click
(define (still->flooding sw flooding-to)
  (local ((define upper-left (upper-left-tile (world-tiles sw)))
          (define unflooded (remove upper-left (still-tiles sw))))
    (make-flooding (list (update-color upper-left flooding-to))
                   unflooded
                   (add1 (still-clicks sw))
                   (tile-color upper-left)
                   flooding-to)))
(check-expect (still->flooding STILL 'green) FLOODING)

;; tile-selection : Natural Natural World -> Tile
;; Determine which tile was clicked
(define (tile-selection x y w)
  (extract-tile (world-tiles w) (make-posn (quotient x CELL-SIZE) (quotient y CELL-SIZE))))
(check-expect (tile-selection 5 6 STILL) UPPER-LEFT-RED)
(check-expect (tile-selection 31 42 STILL) (tyle 1 2 'green))

;;--------------------all-equal?--------------------

;; all-equal? : World -> Boolean
;; Are all the tiles of equal color?
(define (all-equal? w)
  (or (empty? (world-tiles w))
      (local ((define (all-colors-equal? tiles color)
                (andmap (λ (t) (color=? (tile-color t) color)) tiles)))
        (all-colors-equal? (world-tiles w) (tile-color (first (world-tiles w)))))))
(check-expect (all-equal? STILL) #f)
(check-expect (all-equal? (make-still '() 0)) #t)
(check-expect (all-equal? (make-still (list UPPER-LEFT-RED) 0)) #t)
(check-expect (all-equal? (make-still (list (tyle 0 0 'red)
                                            (tyle 0 1 'red)
                                            (tyle 1 0 'red)
                                            (tyle 1 1 'red))
                                      0))
              #t)

;--------------------General utility functions--------------------


;; extract-tile : [List-of Tile] Posn -> Tile
;; Extract the tile with posn p
(define (extract-tile lot p)
  (cond [(posn=? p (tile-posn (first lot))) (first lot)]
        [else (extract-tile (rest lot) p)]))
(check-expect (extract-tile (world-tiles STILL) (make-posn 0 0)) UPPER-LEFT-RED)
(check-expect (extract-tile (world-tiles STILL) (make-posn 1 0)) (tyle 1 0 'red))

;; posn=? : Posn Posn -> Boolean
;; Are the two posns equal?
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))
(check-expect (posn=? (make-posn 0 0) (make-posn 0 0)) #t)
(check-expect (posn=? (make-posn 0 1) (make-posn 0 0)) #f)
(check-expect (posn=? (make-posn 1 0) (make-posn 0 0)) #f)
(check-expect (posn=? (make-posn 1 1) (make-posn 0 0)) #f)

;; upper-left-tile : [List-of Tile] -> Tile
;; Extract the upper left tile
(define (upper-left-tile lot)
  (extract-tile lot (make-posn 0 0)))
(check-expect (upper-left-tile (world-tiles STILL)) UPPER-LEFT-RED)

;--------------------------------------------------------------------

(main 20 9)