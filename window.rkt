#lang racket/base

(require (only-in srfi/1/list pair-for-each)
         racket/gui/base
         framework
         racket/class
         "model.rkt")
(provide fractal-window)

(define-struct (exn:fail:fractal exn:fail) ())

(define (fractal-window [w 400] [h 300])
  (define main-frame%
    (class (frame:basic-mixin frame%)
      (init label (parent #f) (width #f) (height #f))
      (super-new (label label)
                 (parent parent)
                 (width width)
                 (height height))))

  (define frame (new main-frame%
                     (label "Fractal")
                     (width w)
                     (height h)))

  (define contents
    (new vertical-pane%
         (parent (send frame get-area-container))
         (alignment '(center center))
         (stretchable-height #t)))

  (define fractal%
    (class canvas%
      (init (parent #f))
      (inherit set-canvas-background get-dc get-width get-height refresh)

      (define iteration 0)
      (define path null)

      (define (compute-path n width screen-width screen-height)
        (let* ([segment-width (floor (/ width (expt 5 n)))]
               [real-width (* segment-width (expt 5 n))]
               [margin-width (floor (/ (- screen-width real-width) 2))])
          (unless (> segment-width 0)
            (raise (exn:fail:fractal "segment width too small" (current-continuation-marks))))
          (fractal-iteration n (cons margin-width (floor (/ screen-height 2))) segment-width)))

      (define (recompute iteration*)
        (let* ([width (get-width)]
               [height (get-height)]
               ;; give 50 pixels of room on either side
               [picture-width (max (- width 100) 0)]
               [path* (compute-path iteration* picture-width width height)])
          (set! iteration iteration*)
          (set! path path*)
          (let* (;; width(n) = 5^n
                 [min-picture-width (expt 5 iteration*)]
                 ;; give 50 pixels of room on either side
                 [min-screen-width (+ min-picture-width 100)]
                 ;; height(0) = 0, height(n) = 2 * (5^(n-1) + height(n-1))
                 [min-screen-height (+ (* 2 (let f ([n iteration*])
                                              (if (zero? n) 0 (+ (expt 5 (sub1 n))
                                                                 (f (sub1 n))))))
                                       ;; give a little extra buffer room just in case
                                       15)]
                 [area (send frame get-area-container)])
            (send area min-width min-screen-width)
            (send area min-height min-screen-height))))

      (define/public (set-iteration n)
        (recompute n))

      (define/public (increment-iteration)
        (set-iteration (add1 iteration)))

      (define (redraw)
        (with-handlers ([exn:fail:fractal? void])
          (recompute iteration)
          (let ([dc (get-dc)])
            (send dc set-pen "Black" 1 'solid)
            ;; Draw a line segment for every two-element prefix of the path.
            (pair-for-each (lambda (pair)
                             (when (pair? (cdr pair))
                               (let ([point1 (car pair)]
                                     [point2 (cadr pair)])
                                 (send dc draw-line (car point1) (cdr point1) (car point2) (cdr point2)))))
                           path))))

      (define/override (on-event evt)
        (when (send evt button-up?)
          (with-handlers ([exn:fail:fractal? (lambda (exn)
                                               (message-box "Error" "Fractal granularity too high to display." frame '(ok))
                                               (void))])
            (increment-iteration)
            (refresh))))

      (define/override (on-paint)
        (redraw))

      (super-new (parent parent)
                 (stretchable-width #t)
                 (stretchable-height #t)
                 (style '(border)))
      (set-canvas-background (send the-color-database find-color "White"))))

  (define fractal
    (new fractal% (parent contents)))

  (send frame show #t)

  frame)
