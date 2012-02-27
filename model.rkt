#lang racket/base

(require racket/list
         (only-in srfi/1/list list-index))
(provide directions-for path-for fractal-iteration)

;; direction ::= 'N | 'E | 'S | 'W
;; orientation ::= 'CW | 'CCW
;; point ::= (cons nat nat)
;; path ::= (nelistof point)

(define clock '(N E S W))

;; rotate : orientation * direction -> direction
(define (rotate or dir)
  (let ([shift (if (eq? or 'CW) add1 sub1)])
    (list-ref clock (modulo (shift (list-index (lambda (x) (eq? x dir)) clock)) 4))))

;; directions-for : nat * direction -> (listof orientation)
(define (directions-for n dir)
  (if (zero? n)
      (list dir)
      (append-map (lambda (x)
                    (list x (rotate 'CCW x) x (rotate 'CW x) x (rotate 'CW x) x (rotate 'CCW x) x))
                  (directions-for (sub1 n) dir))))

;; path-for : (listof direction) * point * nat -> path
(define (path-for dirs origin len)
  (reverse (foldl (lambda (dir points)
                    (let ([previous (car points)])
                      (let ([x (car previous)]
                            [y (cdr previous)])
                        (cons (case dir
                                [(N) (cons x (- y len))]
                                [(E) (cons (+ x len) y)]
                                [(S) (cons x (+ y len))]
                                [(W) (cons (- x len) y)])
                              points))))
                  (list origin)
                  dirs)))

;; fractal-iteration : nat * point * nat -> path
(define (fractal-iteration n origin len)
  (path-for (directions-for n 'E) origin len))
