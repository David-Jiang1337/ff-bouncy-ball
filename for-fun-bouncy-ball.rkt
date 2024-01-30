;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname for-fun-bouncy-ball) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)
(require 2htdp/universe)

;(@htdw WS)

;; ===========


;; Constants:

(define WIDTH 400)
(define HEIGHT 400)
(define B-COEFF 0.9) ; Bounce coefficient (coefficient of velocity after bounce)
(define GRAVITY 3) ; acceleration due to gravity, positive is down
(define BALL-RADIUS 20)
(define BALL-IMAGE (circle BALL-RADIUS "solid" "black"))
(define MTS (empty-scene WIDTH HEIGHT))


;; Constraints of ball position
(define TOP BALL-RADIUS)
(define BOT (- HEIGHT BALL-RADIUS 1))
(define LEFT BALL-RADIUS)
(define RIGHT (- WIDTH BALL-RADIUS 1))

;; Data Definitions
(@htdd Ball)
(define-struct ball [x y dx dy]) 
;; Ball is (make-ball Number Number Number Number)
;; interp. a ball with position x y and velocity dx dy
;; CONSTRAINT: x within [LEFT, RIGHT], y within [TOP, BOT]

(define B1 (make-ball LEFT (+ TOP 0) 1 0))
(define B2 (make-ball RIGHT (+ TOP 0) -4 0))

(@dd-template-rules compound) ; 4 fields

(define (fn-for-ball b)
  (... (ball-x b)
       (ball-y b)
       (ball-dx b)
       (ball-dy b)))

(@htdd ListOfBall)

;; ListOfBall is one of:
;;  - empty
;;  - (cons Ball ListOfBall)
;; interp. a list of Ball

(define LoB0 (cons B1 (cons B2 empty)))
(define LoB1 empty)
(define LoB2 (cons (make-ball 0 0 0 0) empty))
(define LoB3 (cons (make-ball 0 0 0 0) (cons (make-ball 1 1 2 2) empty)))


(@dd-template-rules one-of
                    atomic-distinct ; empty
                    compound        ; (cons Ball ListOfBall) is ListOfBall
                    ref             ; (first ListOfBall) is Ball
                    self-ref)       ; (rest ListOfBall) is ListOfBall

(@template-origin ListOfBall)

(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]
        [else (... (fn-for-ball (first lob))
                   (fn-for-lob (rest lob)))]))


(@htdf main)
(@signature Ball -> Ball)
;; starts world with (main B1)

(@template-origin htdw-main)

(define (main b)
  (big-bang b                 ; Ball
    (on-tick tock)            ; Ball -> Ball
    (to-draw render)          ; Ball -> Image
    (on-mouse handle-mouse))) ; ListOfBall Integer Integer MouseEvent -> ListOfBall

(@htdf tock)
(@signature ListOfBall -> ListOfBall)
;; Produce ListOfBall with all Balls updated for next tick

(check-expect (tock LoB1) empty)
(check-expect (tock LoB2)
              (cons (next-ball (make-ball 0 0 0 0)) empty))
(check-expect (tock LoB3)
              (cons (next-ball (make-ball 0 0 0 0)) (cons (next-ball (make-ball 1 1 2 2)) empty)))

;(define (tock lob) lob) ;stub

(@template-origin ListOfBall)

(@template
 (define (tock lob)
   (cond [(empty? lob) (...)]
         [else (... (fn-for-ball (first lob))
                    (tock (rest lob)))])))

(define (tock lob)
  (cond [(empty? lob) empty]
        [else
         (cons (next-ball (first lob)) (tock (rest lob)))]))

(@htdf next-ball)
(@signature Ball -> Ball)
;; Produce Balls states for next tick

;(define (next-ball b) b)  ;stub

(@template-origin Ball)

(@template
 (define (next-ball b)
   (... b)))

(define (next-ball b)
  (cond
    [(and (collide-left? b) (collide-bottom? b)) (bounce-bottom(bounce-left b))]
    [(collide-left? b) (bounce-left b)]
    [(collide-right? b) (bounce-right b)]
    [(collide-bottom? b) (bounce-bottom b)]
    [else (move-ball b)]))

(@htdf collide-left?)
(@signature Ball -> Ball)
;; returns whether the ball will pass (or touch) LEFT constraint
(check-expect (collide-left? (make-ball (+ LEFT 9) 0 -10 0)) true)
(check-expect (collide-left? (make-ball (+ LEFT 10) 0 -10 0)) true)
(check-expect (collide-left? (make-ball (+ LEFT 11) 0 -10 0)) false)

;(define (collide-left? b) false) ;stub

(@template-origin Ball)

(@template
 (define (collide-left? b)
   (... (ball-x b)
        (ball-y b)
        (ball-dx b)
        (ball-dy b))))

(define (collide-left? b)
  (<= (+ (ball-x b) (ball-dx b)) LEFT))

(@htdf collide-right?)
(@signature Ball -> Ball)
;; returns whether the ball will pass (or touch) LEFT constraint
(check-expect (collide-right? (make-ball (- RIGHT 9) 0 10 0)) true)
(check-expect (collide-right? (make-ball (- RIGHT 10) 0 10 0)) true)
(check-expect (collide-right? (make-ball (- RIGHT 11) 0 10 0)) false)

;(define (collide-right? b) false) ;stub

(@template-origin Ball)

(@template
 (define (collide-right? b)
   (... (ball-x b)
        (ball-y b)
        (ball-dx b)
        (ball-dy b))))

(define (collide-right? b)
  (>= (+ (ball-x b) (ball-dx b)) RIGHT))

(@htdf collide-bottom?)
(@signature Ball -> Ball)
;; returns whether the ball will pass (or touch) LEFT constraint
(check-expect (collide-bottom? (make-ball 0 (- BOT 9) 0 10)) true)
(check-expect (collide-bottom? (make-ball 0 (- BOT 10) 0 10)) true)
(check-expect (collide-bottom? (make-ball 0 (- BOT 11) 0 10)) false)

;(define (collide-bottom? b) false) ;stub

(@template-origin Ball)

(@template
 (define (collide-bottom? b)
   (... (ball-x b)
        (ball-y b)
        (ball-dx b)
        (ball-dy b))))

(define (collide-bottom? b)
  (>= (+ (ball-y b) (ball-dy b)) BOT))

(@htdf bounce-left)
(@signature Ball -> Ball)
;; returns a ball that moves to the left wall and then bounces with the remaining speed, reverses sign of dx
;; CONSTRAINT: should be bound to touch or pass left wall on next tick
(check-expect (bounce-left (make-ball (+ LEFT 10) 100 -15 5))
              (make-ball (+ LEFT 5) 105 15 (+ GRAVITY 5)))
(check-expect (bounce-left (make-ball (+ LEFT 5) 100 -10 -5))
              (make-ball (+ LEFT 5) 95 10 (+ GRAVITY -5)))
(check-expect (bounce-left (make-ball (+ LEFT 15) 100 -15 0))
              (make-ball (+ LEFT 0) 100 15 GRAVITY))

;(define (bounce-left b) b) ;stub

(@template-origin Ball)

(@template
 (define (bounce-left b)
   (... (ball-x b)
        (ball-y b)
        (ball-dx b)
        (ball-dy b))))

(define (bounce-left b)
  (make-ball
   (- (* 2 LEFT) (ball-dx b) (ball-x b))
   (+ (ball-y b) (ball-dy b))
   (- 0 (ball-dx b))
   (+ GRAVITY (ball-dy b))))

(@htdf bounce-right)
(@signature Ball -> Ball)
;; returns a ball that moves to the right wall and then bounces with the remaining speed, reverses sign of dx
(check-expect (bounce-right (make-ball (- RIGHT 10) 100 15 5))
              (make-ball (- RIGHT 5) 105 -15 (+ GRAVITY 5)))
(check-expect (bounce-right (make-ball (- RIGHT 5) 100 10 -5))
              (make-ball (- RIGHT 5) 95 -10 (+ GRAVITY -5)))
(check-expect (bounce-right (make-ball (- RIGHT 15) 100 15 0))
              (make-ball (- RIGHT 0) 100 -15 GRAVITY))

;(define (bounce-right b) b) ;stub

(@template-origin Ball)

(@template
 (define (bounce-right b)
   (... (ball-x b)
        (ball-y b)
        (ball-dx b)
        (ball-dy b))))

(define (bounce-right b)
  (make-ball
   (- (* 2 RIGHT) (ball-dx b) (ball-x b))
   (+ (ball-y b) (ball-dy b))
   (- 0 (ball-dx b))
   (+ GRAVITY (ball-dy b))))

(@htdf bounce-bottom)
(@signature Ball -> Ball)
;; returns a ball that moves to the bottom wall and then bounces with the remaining speed, reverses sign of dy, multiplies dy by B-COEFF
(check-expect (bounce-bottom (make-ball 100 (- BOT 10) 5 15))
              (make-ball 105 (- BOT 5) 5 (+ (* B-COEFF -15) GRAVITY)))
(check-expect (bounce-bottom (make-ball 100 (- BOT 5) -5 10))
              (make-ball 95 (- BOT 5) -5 (+ (* B-COEFF -10) GRAVITY)))
(check-expect (bounce-bottom (make-ball 100 (- BOT 15) 0 15))
              (make-ball 100 (- BOT 0) 0 (+ (* B-COEFF -15) GRAVITY)))

;(define (bounce-bottom b) b) ;stub

(@template-origin Ball)

(@template
 (define (bounce-bottom b)
   (... (ball-x b)
        (ball-y b)
        (ball-dx b)
        (ball-dy b))))

(define (bounce-bottom b)
  (make-ball
   (+ (ball-x b) (ball-dx b))
   (- (* 2 BOT) (ball-dy b) (ball-y b))
   (ball-dx b)
   (+ (* B-COEFF (- 0 (ball-dy b))) GRAVITY)))

(@htdf move-ball)
(@signature Ball -> Ball)
;; adds dx to x, adds dy to y, adds GRAVITY to dy
(check-expect (move-ball (make-ball 100 200 10 20))
              (make-ball 110 220 10 (+ 20 GRAVITY)))
(check-expect (move-ball (make-ball 100 200 0 0))
              (make-ball 100 200 0 GRAVITY))

;(define (move-ball b) b) ;stub

(@template-origin Ball)

(@template
 (define (move-ball b)
   (... (ball-x b)
        (ball-y b)
        (ball-dx b)
        (ball-dy b))))

(define (move-ball b)
  (make-ball
   (+ (ball-x b) (ball-dx b))
   (+ (ball-y b) (ball-dy b))
   (ball-dx b)
   (+ (ball-dy b) GRAVITY)))

(@htdf floor-coefficient)
(@signature Ball -> Boolean)
;; returns 1 if dy after bounce is negative, else return 0
;; enter the ball before it has undergone bounce-bottom
(check-expect (floor-coefficient (make-ball 100 (- BOT 1 ) 0 (- (/ GRAVITY B-COEFF) 1))) 0)
(check-expect (floor-coefficient (make-ball 100 (- BOT 1 ) 0 (/ GRAVITY B-COEFF))) 0)
(check-expect (floor-coefficient (make-ball 100 (- BOT 1 ) 0 (+ (/ GRAVITY B-COEFF) 1))) 1)

;(define (floor-coefficient b) 0);

(@template-origin Ball)

(@template
 (define (floor-coefficient b) 
   (... (ball-x b)
        (ball-y b)
        (ball-dx b)
        (ball-dy b))))

(define (floor-coefficient b) 
  (cond [(< (+ (* B-COEFF (- 0 (ball-dy b))) GRAVITY) 0) 1]
        [else 0]))

(@htdf handle-mouse)
(@signature ListOfBall Integer Integer MouseEvent -> ListOfBall)
;; add ball with random dx at mouse position to ListOfBall when mouse down
(define R1 (- (random 11) 5))
 
;(check-expect (handle-mouse empty 100 100 "button-down")
;              (cons (make-ball 100 100 R1 0) empty))
(check-expect (handle-mouse (cons (make-ball 100 100 R1 0) empty) 100 100 "drag")
              (cons (make-ball 100 100 R1 0) empty))

;(define (handle-mouse lob x y me) lob) ;stub

(@template-origin MouseEvent)

(@template
 (define (handle-mouse lob x y me)
   (cond [(mouse=? me "button-down") (... loe x y)]
         [else
          (... loe x y)])))

(define (handle-mouse lob x y me)
   (cond [(mouse=? me "button-down")
          (cons (make-ball x y (- (random 11) 5) 0) lob)]
         [else lob]))


(@htdf render)
(@signature ListOfBall -> Image)
;; produce image with all Balls in list at appropriate x and y over MTS
;; CONSTRAINT: ball-dx within [-5, 5]

(check-expect (render (cons (make-ball 100 200 0 0) empty))
              (place-image BALL-IMAGE 100 200 MTS))
(check-expect (render (cons (make-ball 0 0 0 0) (cons (make-ball 100 100 2 2) empty)))
              (place-image BALL-IMAGE 100 100
                           (place-image BALL-IMAGE 0 0 MTS)))

;(define (render lob) MTS) ;stub

(@template-origin ListOfBall)

(@template
 (define (render lob)
   (cond [(empty? lob) (...)]
         [else (... (fn-for-ball (first lob))
                    (render (rest lob)))])))

(define (render lob)
  (cond [(empty? lob) MTS]
        [else
         (place-image
          BALL-IMAGE
          (ball-x (first lob))
          (ball-y (first lob))
          (render (rest lob)))]))