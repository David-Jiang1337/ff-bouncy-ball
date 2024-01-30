;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname for-fun-bouncy-ball) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)
(require 2htdp/universe)

;(@htdw WS)

;; ===========



(define (fn-for-ball b)
  (... (ball-x b)
       (ball-y b)
       (ball-dx b)
       (ball-dy b)))

;; Constants:

(define WIDTH 400)
(define HEIGHT 400)
(define B-COEFF 0.9) ; Bounce coefficient (coefficient of velocity after bounce)
(define GRAVITY 4) ; acceleration due to gravity, positive is down
(define BALL-RADIUS 20)
(define BALL-IMAGE (circle BALL-RADIUS "solid" "black"))
(define MTS (empty-scene WIDTH HEIGHT))


;; Constraints of ball position
(define TOP BALL-RADIUS)
(define BOT (- HEIGHT BALL-RADIUS 1))
(define LEFT BALL-RADIUS)
(define RIGHT (- WIDTH BALL-RADIUS 1))

;; Local types
(@htdd Ball)
(define-struct ball [x y dx dy]) ;num num num num
(define B1 (make-ball TOP (+ TOP 100) 10 0))
;; ===========
;; Data Definitions

;(@htdd BallX)
;;; BallX is number
;;; interp. x position of ball
;;; constraint [LEFT, RIGHT]
;
;;(define X1 22)
;;(define X2 RIGHT)
;
;(@dd-template-rules atomic-non-distinct)
;
;(define (fn-for-ballx x)
;  (... x))

(@htdf main)
(@signature Ball -> Ball)
;; starts world with (main B1)

(@template-origin htdw-main)

(define (main b)
  (big-bang b           ; Ball
    (on-tick next-ball)      ; Ball -> Ball
    (to-draw render)))  ; Ball -> Image


(@htdf next-ball)
(@signature Ball -> Physic)
;; Produce Balls states for next tick
;(check-expect (next-ball
;               (make-ball (/ WIDTH 2) TOP 10 0))
;              (make-ball (+ (/ WIDTH 2) 10) (+ TOP 10) 10 (+ GRAVITY 0)))
;(check-expect (next-ball
;               (make-ball (/ WIDTH 2) (+ BOT 20) 10 200))
;              (make-ball (+ (/ WIDTH 2) 10) (- BOT 180) 10 (* (- 0 200) B-COEFF)))
;(check-expect (next-ball (make-ball (- RIGHT 6) TOP -10 0))
;              (make-ball (- RIGHT 4) (+ TOP 10) 10 (+ GRAVITY 0)))
;(check-expect (next-ball (make-ball (- RIGHT 4) TOP 10 0))
;              (make-ball (- RIGHT 6) (+ TOP 10) -10 (+ GRAVITY 0)))
;(check-expect (next-ball (make-ball (+ LEFT 6) TOP -10 0))
;              (make-ball (+ LEFT 4) (+ TOP 10) 10 (+ GRAVITY 0)))
;(check-expect (next-ball (make-ball (- RIGHT 4) TOP -10 0))
;              (make-ball (- RIGHT 6) (+ TOP 10) 10 (+ GRAVITY 0)))


;(define (next-ball b) b)  ;stub

(@template-origin Ball)

(@template
 (define (next-ball b)
   (... b)))

(define (next-ball b)
  (cond [(collide-left? b) (bounce-left b)]
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

(@htdf render)
(@signature Ball -> Image)
;; produce image with ball placed on MTS at proper x, y position
(check-expect (render (make-ball 100 200 10 20)) (place-image BALL-IMAGE 100 200 MTS))
;(define (render b) MTS) ;stub

(@template-origin Ball)

(@template
 (define (render b)
   (... (ball-x b)
        (ball-y b)
        (ball-dx b)
        (ball-dy b))))

(define (render b)
  (place-image BALL-IMAGE (ball-x b) (ball-y b) MTS))

(main B1)