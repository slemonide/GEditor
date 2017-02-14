;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname geditor) (read-case-sensitive #t) (teachpacks ((lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.ss" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Conway's game of life
;;
;;
;; enter creates a cell
;; backspace removes a cell
;; arrow keys move cursor
;; c cleans the workspace
;; p pauses/unpauses the world

;; =================
;; Constants:

(define WIDTH 800)
(define HEIGHT 600)
(define MTS (empty-scene WIDTH HEIGHT))

(define COLOR "green")
(define SIZE 10)
(define CELL (square SIZE "solid" COLOR))

(define CURSOR (square SIZE "outline" "blue"))

;; =================
;; Data definitions:

(define-struct cell (x y))
;; Cell is (make-cell Natural Natural)
;; interp. a cell's position relative to the upper left edge, measured in SIZE

(define P1 (make-cell 100 23))
(define P2 (make-cell 400 200))
#;
(define (fn-for-cell p)
  (... (cell-x p)
       (cell-y p)))

;; Template rules used:
;;  - compound: 2 fields


;; ListOfCell is one of:
;;  - empty
;;  - (cons cell ListOfCell)
;; interp. list of cells

(define LOC1 empty)
(define LOC2 (cons P1 (cons P2 empty)))
#;
(define (fn-for-loc loc)
  (cond [(empty? loc) (...)]
        [else
         (... (fn-for-cell (first loc))
              (fn-for-loc (rest loc)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compond: (cons cell ListOfCell)
;;  - reference: (first loc)
;;  - self-reference (rest loc) is ListOfCell

(define-struct window (cursor picture pause?))
;; Window is (make-window cell ListOfCell Boolean)
;; interp. the window of the editor, including the picture,
;;         a cursor position, and the state of an ediror

(define W1 (make-window P1 LOC1 true))
(define W2 (make-window P2 LOC2 false))
#;
(define (fn-for-window w)
  (... (fn-for-cell (window-cursor w))
       (fn-for-loc (window-picture w))
       (window-pause? w)))

;; Template rules used:
;;  - compound: 2 fields
;;  - reference: (window-cursor s)
;;  - reference: (window-picture s)
;;  - atomic non-distinct: (window-pause? w) is Boolean

;; =================
;; Functions:

;; Window -> Window
;; start the world with (run-editor empty)

(define (run-editor picture)
  (big-bang (make-window (make-cell (/ (/ WIDTH 2) SIZE)
                                    (/ (/ HEIGHT 2) SIZE))
                         picture true)
            (to-draw render)        ; Window -> Image
            (on-tick update 0.1)        ; Window -> Window
            ;(on-mouse handle-mouse) ; Window Natural Natural MouseEvent -> Window
            (on-key handle-key)))   ; Window KeyEvent -> Window

;; Window -> Image
;; render the picture and a cursor
(check-expect (render (make-window (make-cell 1 5) LOC1 true))
              (place-image CURSOR (* SIZE 1) (* SIZE 5) (make-picture LOC1)))
(check-expect (render (make-window (make-cell 10 20) LOC1 false))
              (place-image CURSOR (* SIZE 10) (* SIZE 20) (make-picture LOC1)))

;(define (render w) MTS) ;stub

;<template from Window>
(define (render w)
  (place-image CURSOR
               (* SIZE (cell-x (window-cursor w)))
               (* SIZE (cell-y (window-cursor w)))
               (make-picture (window-picture w))))

;; ListOfCells -> Image
;; draw a picture using given list of cells
(check-expect (make-picture empty) MTS)
(check-expect (make-picture (cons (make-cell 10 20) empty))
              (place-image CELL (* SIZE 10) (* SIZE 20) MTS))
(check-expect (make-picture (cons (make-cell 14 10) empty))
              (place-image CELL (* SIZE 14) (* SIZE 10) MTS))

;(define (make-picture loc) MTS) ;stub

;<template from ListOfCell>
(define (make-picture loc)
  (cond [(empty? loc) MTS]
        [else
         (place-cell (first loc)
                     (make-picture (rest loc)))]))

;; Window -> Window
;; Update the window following "Game of Life" rules if the game is unpaused
;; !!!

;(define (update w) w) ;stub

;<Template from Window>

(define (update w)
  (if (not (window-pause? w))
      (make-window (window-cursor w)
                   (play-life (window-picture w))
                   (window-pause? w))
      w))

;; ListOfCell -> ListOfCell
;; delete repeating entities from the list of cell
(check-expect (soap empty) empty)
(check-expect (soap (cons (make-cell 32 43)
                          empty))
              (cons (make-cell 32 43)
                    empty))
(check-expect (soap (cons (make-cell 32 43)
                          (cons (make-cell 32 43)
                                empty)))
              (cons (make-cell 32 43)
                    empty))
(check-expect (soap (cons (make-cell 32 43)
                          (cons (make-cell 54 21)
                                empty)))
              (cons (make-cell 32 43)
                    (cons (make-cell 54 21)
                          empty)))

;(define (soap loc) loc);stub 

;<Template from ListOfCell>
(define (soap loc)
  (cond [(empty? loc) empty]
        [else
         (if (contains-cell? (first loc) (rest loc))
             (soap (rest loc))
             (cons (first loc)
                   (soap (rest loc))))]))

;; Cell ListOfCell -> Boolean
;; return true if a given cell is in the given list of cells
(check-expect (contains-cell? (make-cell 23 14) empty) false)
(check-expect (contains-cell? (make-cell 44 21)
                              (cons (make-cell 32 14) empty)) false)
(check-expect (contains-cell? (make-cell 44 21)
                              (cons (make-cell 32 14)
                                    (cons (make-cell 22 43)
                                          empty))) false)
(check-expect (contains-cell? (make-cell 44 21)
                              (cons (make-cell 44 21)
                                    (cons (make-cell 23 13)
                                          empty))) true)
(check-expect (contains-cell? (make-cell 44 21)
                              (cons (make-cell 23 13)
                                    (cons (make-cell 44 21)
                                          empty))) true)

;(define (contains-cell? p loc) false)

;<Template from ListOfCell with additional compound parameter>
(define (contains-cell? p loc)
  (cond [(empty? loc) false]
        [else
         (or (cell=? p (first loc))
             (contains-cell? p (rest loc)))]))

;; ListOfCell -> ListOfCell
;; Update the list of cells following the Conway's "Game of Life" rules
;(check-expect (play-life LOC2) (append (clean-life LOC2)
;                                       (summon-cells LOC2)))

(define (play-life loc)
  (append (clean-life loc)
          (summon-cells loc)))

;(define (play-life loc)
;  (append (clean-life loc)
;          (summon-cells loc)))

;; ListOfCell -> ListOfCell
;; Delete cells following the Conway's "Game of Life" rules
(check-expect (clean-life empty) empty)
(check-expect (clean-life (cons (make-cell 10 30)
                                empty)) empty)
(check-expect (clean-life (cons (make-cell 10 30)
                                (cons (make-cell 45 23)
                                      empty))) empty)

(check-expect (clean-life (cons (make-cell 10 10)
                                (cons (make-cell 10 11)
                                      (cons (make-cell 11 11)
                                            (cons (make-cell 10 11)
                                                  empty)))))
              (cons (make-cell 10 10)
                    (cons (make-cell 10 11)
                          (cons (make-cell 11 11)
                                (cons (make-cell 10 11)
                                      empty)))))
(check-expect (clean-life (list (make-cell 40 28) (make-cell 41 28) (make-cell 42 28) (make-cell 43 28) (make-cell 44 28)
                                (make-cell 44 30) (make-cell 43 30) (make-cell 42 30) (make-cell 41 30) (make-cell 40 30)))
              (list (make-cell 41 28) (make-cell 42 28) (make-cell 43 28)
                    (make-cell 43 30) (make-cell 42 30) (make-cell 41 30)))

(define (clean-life loc)
  (local [(define (clean-life-helper loc-current loc-full)
            (cond [(empty? loc-current) empty]
                  [else
                   (cond [(or (< (neighbours (first loc-current) loc-full) 2)
                              (> (neighbours (first loc-current) loc-full) 3))
                          (clean-life-helper (rest loc-current) loc-full)]
                         [(<= 2 (neighbours (first loc-current) loc-full) 3)
                          (cons (first loc-current)
                                (clean-life-helper (rest loc-current) loc-full))])]))]
    
    (clean-life-helper loc loc)))

;; ListOfCell -> ListOfCell
;; create a list of cells to be added based on Conway's "Game of Life" rules
(check-expect (summon-cells empty) empty)
(check-expect (summon-cells (cons (make-cell 10 20) empty)) empty)
(check-expect (summon-cells (cons (make-cell 10 20)
                                  (cons (make-cell 11 20)
                                        (cons (make-cell 12 20) empty))))
              (cons (make-cell 11 19)
                    (cons (make-cell 11 21) empty)))

; template from ListOfCell
(define (summon-cells loc)
  (local [(define (summon-cells-helper loc-current loc-full)
            (cond [(empty? loc-current) empty]
                  [else
                   (if (empty? (summon-cells-at (first loc-current) loc-full))
                       (summon-cells-helper (rest loc-current) loc-full)
                       (append (summon-cells-at (first loc-current) loc-full)
                               (summon-cells-helper (rest loc-current) loc-full)))]))]
    (soap (summon-cells-helper loc loc))))

;; Cell ListOfCell -> ListOfCell
;; create a list containing cells to be spawned near a given cell, but not at the cell
(check-expect (summon-cells-at (make-cell 32 43) empty) empty)
(check-expect (summon-cells-at (make-cell 11 20)
                               (cons (make-cell 10 20)
                                     (cons (make-cell 11 20)
                                           (cons (make-cell 12 20)
                                                 empty))))
              (cons (make-cell 11 21)
                    (cons (make-cell 11 19)
                          empty)))

;(define (summon-cells-at p loc) empty) ;stub

;;<Template from cell with additional self-referential parameter>
(define (summon-cells-at p loc)
  (local [(define (spawn-cells p n loc)
            (cond [(zero? n) empty]
                  [else
                   (if (contains-cell? (adjacent-cell p n) loc)
                       (spawn-cells p (sub1 n) loc)
                       (if (= (neighbours (adjacent-cell p n) loc) 3)
                           (cons (adjacent-cell p n)
                                 (spawn-cells p (sub1 n) loc))
                           (spawn-cells p (sub1 n) loc)))]))]
    (spawn-cells p 8 loc)))

;; Cell Natural -> Cell
;; produce a cell adjacent to the given cell depending on n by the following rules:
;;   - n = 1 means bottom left cell
;;   - n = 2 means left cell
;;   - ...
;;   - n = 8 means bottom cell
(check-expect (adjacent-cell (make-cell 10 20) 1)
              (make-cell (sub1 10) (add1 20)))
(check-expect (adjacent-cell (make-cell 10 20) 2)
              (make-cell (sub1 10)       20))
(check-expect (adjacent-cell (make-cell 10 20) 3)
              (make-cell (sub1 10) (sub1 20)))
(check-expect (adjacent-cell (make-cell 10 20) 4)
              (make-cell       10  (sub1 20)))
(check-expect (adjacent-cell (make-cell 10 20) 5)
              (make-cell (add1 10) (sub1 20)))
(check-expect (adjacent-cell (make-cell 10 20) 6)
              (make-cell (add1 10)       20))
(check-expect (adjacent-cell (make-cell 10 20) 7)
              (make-cell (add1 10) (add1 20)))
(check-expect (adjacent-cell (make-cell 10 20) 8)
              (make-cell       10  (add1 20)))

;(define (adjacent-cell p n) p) ;stub

#;
(define (adjacent-cell p n)     ;template
  (cond [(= n 1) (... p)]
        [(= n 2) (... p)]
        [(= n 3) (... p)]
        [(= n 4) (... p)]
        [(= n 5) (... p)]
        [(= n 6) (... p)]
        [(= n 7) (... p)]
        [(= n 8) (... p)]))

(define (adjacent-cell p n)
  (cond [(= n 1) (cell-left (cell-down p))]
        [(= n 2) (cell-left p)]
        [(= n 3) (cell-left (cell-up p))]
        [(= n 4) (cell-up p)]
        [(= n 5) (cell-right (cell-up p))]
        [(= n 6) (cell-right p)]
        [(= n 7) (cell-right (cell-down p))]
        [(= n 8) (cell-down p)]))


;; Cell ListOfCell -> Natural
;; return the number of adjacent cells to a given cell in the given loc
(check-expect (neighbours (make-cell 40 30) empty) 0)
(check-expect (neighbours (make-cell 40 30)
                          (cons (make-cell 20 99) empty)) 0)
(check-expect (neighbours (make-cell 40 30)
                          (cons (make-cell 40 30) empty)) 0)
(check-expect (neighbours (make-cell 40 30)
                          (cons (make-cell 41 30) empty)) 1)
(check-expect (neighbours (make-cell 40 30)
                          (cons (make-cell 41 30)
                                (cons (make-cell 40 31)
                                      empty))) 2)
(check-expect (neighbours (make-cell 40 30)
                          (cons (make-cell 41 30)
                                (cons (make-cell 40 31)
                                      (cons (make-cell 41 31)
                                            empty)))) 3)

;(define (neighbours p loc) 0) ;stub

;<Template from ListOfCell with additional compound paramenter>
(define (neighbours p loc)
  (cond [(empty? loc) 0]
        [else
         (if (adjacent-cells? p (first loc))
             (add1 (neighbours p (rest loc)))
             (neighbours p (rest loc)))]))

;; Cell Cell -> Boolean
;; return true if two given cells are adjacent to each other
(check-expect (adjacent-cells? (make-cell 20 30)
                               (make-cell 99 89))
              false)
(check-expect (adjacent-cells? (make-cell 20 30)
                               (make-cell 21 30))
              true)
(check-expect (adjacent-cells? (make-cell 20 30)
                               (make-cell 21 31))
              true)
(check-expect (adjacent-cells? (make-cell 20 30)
                               (make-cell 20 31))
              true)
(check-expect (adjacent-cells? (make-cell 20 30)
                               (make-cell 20 30))
              false)
(check-expect (adjacent-cells? (make-cell 20 30)
                               (make-cell 19 30))
              true)
(check-expect (adjacent-cells? (make-cell 20 30)
                               (make-cell 20 29))
              true)
(check-expect (adjacent-cells? (make-cell 20 30)
                               (make-cell 19 29))
              true)

;(define (adjacent-cells? p1 p2) false) ;stub

;<Modified template from cell>
(define (adjacent-cells? p1 p2)
  (and (not (cell=? p1 p2))
       (<= (abs (- (cell-x p1) (cell-x p2))) 1)
       (<= (abs (- (cell-y p1) (cell-y p2))) 1)))

;; Cell Image -> Image
;; Draw a given cell on the given image
(check-expect (place-cell (make-cell 20 30) MTS)
              (place-image CELL (* SIZE 20) (* SIZE 30) MTS))

;(define (place-cell p img) img) ;stub

;<template from cell with additional atomic paramater>
(define (place-cell p img)
  (place-image CELL
               (* SIZE (cell-x p))
               (* SIZE (cell-y p))
               img))

;; Window KeyEvent -> Window
;; move the cursor and add cell when keys are pressed
;; no examples here, sorry
;(define (handle-key w ke) w) ;stub
(check-expect (handle-key W1 "\r") (draw W1))
(check-expect (handle-key W1 "c") (purge W1))
(check-expect (handle-key W1 "p") (pause W1))
(check-expect (handle-key W1 "q") (stop W1))
(check-expect (handle-key W1 "\b") (erase W1))
(check-expect (handle-key W1 "left") (move-left W1))
(check-expect (handle-key W1 "right") (move-right W1))
(check-expect (handle-key W1 "up") (move-up W1))
(check-expect (handle-key W1 "down") (move-down W1))
(check-expect (handle-key W1 "q") W1)

(check-expect (handle-key W2 "\r") (draw W2))
(check-expect (handle-key W2 "c") (purge W2))
(check-expect (handle-key W2 "p") (pause W2))
(check-expect (handle-key W2 "q") (stop W2))
(check-expect (handle-key W2 "\b") (erase W2))
(check-expect (handle-key W2 "left") (move-left W2))
(check-expect (handle-key W2 "right") (move-right W2))
(check-expect (handle-key W2 "up") (move-up W2))
(check-expect (handle-key W2 "down") (move-down W2))
(check-expect (handle-key W2 "i") W2)

(define (handle-key w ke)
  (cond [(key=? ke "\r") (draw w)]
        [(key=? ke "c") (purge w)]
        [(key=? ke "p") (pause w)]
        [(key=? ke "q") (stop w)]
        [(key=? ke "\b") (erase w)]
        [(key=? ke "left") (move-left w)]
        [(key=? ke "right") (move-right w)]
        [(key=? ke "up") (move-up w)]
        [(key=? ke "down") (move-down w)]
        [else w]))

;; Window -> Window
;; add a cell at a current coursor position, unless the space is occupied
(check-expect (draw (make-window (make-cell 40 30)
                                 empty
                                 false))
              (make-window (make-cell 40 30)
                           (cons (make-cell 40 30)
                                 empty)
                           false))
(check-expect (draw (make-window (make-cell 40 30)
                                 (cons (make-cell 50 20)
                                       empty)
                                 false))
              (make-window (make-cell 40 30)
                           (cons (make-cell 40 30)
                                 (cons (make-cell 50 20)
                                       empty))
                           false))

;(define (draw w) w) ;stub

;<Template from Window>
(define (draw w)
  (make-window (window-cursor w)
               (if (contains-cell? (window-cursor w)
                                   (window-picture w))
                   (window-picture w)
                   (cons (window-cursor w)
                         (window-picture w)))
               (window-pause? w)))

;; Window -> Window
;; remove a cell at a current cursor position
(check-expect (erase (make-window (make-cell 40 30)
                                  empty
                                  false))
              (make-window (make-cell 40 30)
                           empty
                           false))
(check-expect (erase (make-window (make-cell 50 20)
                                  (cons (make-cell 50 20)
                                        empty)
                                  false))
              (make-window (make-cell 50 20)
                           empty
                           false))
(check-expect (erase (make-window (make-cell 50 20)
                                  (cons (make-cell 50 20)
                                        (cons (make-cell 23 76)
                                              empty))
                                  false))
              (make-window (make-cell 50 20)
                           (cons (make-cell 23 76)
                                 empty)
                           false))
(check-expect (erase (make-window (make-cell 50 20)
                                  (cons (make-cell 23 76)
                                        (cons (make-cell 50 20)
                                              empty))
                                  false))
              (make-window (make-cell 50 20)
                           (cons (make-cell 23 76)
                                 empty)
                           false))

;(define (erase w) w) ;stub

;<Template from Window>
(define (erase w)
  (make-window (window-cursor w)
               (remove-cell (window-cursor w)
                            (window-picture w))
               (window-pause? w)))

;; Cell ListOfCell -> ListOfCell
;; remove all the cells in the given list that match a given cell
(check-expect (remove-cell (make-cell 32 13) empty) empty)
(check-expect (remove-cell (make-cell 32 13)
                           (cons (make-cell 21 40) empty))
              (cons (make-cell 21 40) empty))
(check-expect (remove-cell (make-cell 32 13)
                           (cons (make-cell 21 40)
                                 (cons (make-cell 44 92) empty)))
              (cons (make-cell 21 40)
                    (cons (make-cell 44 92) empty)))

(check-expect (remove-cell (make-cell 32 13)
                           (cons (make-cell 32 13) empty))
              empty)
(check-expect (remove-cell (make-cell 32 13)
                           (cons (make-cell 32 13)
                                 (cons (make-cell 44 92) empty)))
              (cons (make-cell 44 92) empty))
(check-expect (remove-cell (make-cell 32 13)
                           (cons (make-cell 44 92)
                                 (cons (make-cell 32 13) empty)))
              (cons (make-cell 44 92) empty))

;(define (remove-cell p loc) empty) ;stub

;<Template from ListOfCell with an additional compound paramater>
(define (remove-cell p loc)
  (cond [(empty? loc) empty]
        [else
         (if (cell=? p (first loc))
             (remove-cell p (rest loc))
             (cons (first loc)
                   (remove-cell p (rest loc))))]))

;; Window -> Window
;; purge the world from cells
(check-expect (purge (make-window (make-cell 32 12)
                                  empty
                                  false))
              (make-window (make-cell 32 12)
                           empty
                           false))
(check-expect (purge (make-window (make-cell 32 52)
                                  (cons (make-cell 30 40)
                                        empty)
                                  false))
              (make-window (make-cell 32 52)
                           empty
                           false))
(check-expect (purge (make-window (make-cell 33 21)
                                  (cons (make-cell 30 40)
                                        (cons (make-cell 52 30)
                                              empty))
                                  false))
              (make-window (make-cell 33 21)
                           empty
                           false))

;(define (purge w) w) ;stub

;<Template from Window>
(define (purge w)
  (make-window (window-cursor w)
               empty
               (window-pause? w)))

;; Window -> Window
;; pause or unpause the game
(check-expect (pause (make-window (make-cell 33 12)
                                  empty
                                  false))
              (make-window (make-cell 33 12)
                           empty
                           true))
(check-expect (pause (make-window (make-cell 32 52)
                                  (cons (make-cell 30 40)
                                        empty)
                                  true))
              (make-window (make-cell 32 52)
                           (cons (make-cell 30 40)
                                 empty)
                           false))
(check-expect (pause (make-window (make-cell 33 21)
                                  (cons (make-cell 30 40)
                                        (cons (make-cell 52 30)
                                              empty))
                                  false))
              (make-window (make-cell 33 21)
                           (cons (make-cell 30 40)
                                 (cons (make-cell 52 30)
                                       empty))
                           true))

;(define (pause w) w) ;stub

;<Template from Window>
(define (pause w)
  (make-window (window-cursor w)
               (window-picture w)
               (not (window-pause? w))))

;; Window -> Window
;; pause the game, do nothing if the game is already paused
(check-expect (stop (make-window (make-cell 33 12)
                                 empty
                                 false))
              (make-window (make-cell 33 12)
                           empty
                           true))
(check-expect (stop (make-window (make-cell 32 52)
                                 (cons (make-cell 30 40)
                                       empty)
                                 true))
              (make-window (make-cell 32 52)
                           (cons (make-cell 30 40)
                                 empty)
                           true))
(check-expect (stop (make-window (make-cell 33 21)
                                 (cons (make-cell 30 40)
                                       (cons (make-cell 52 30)
                                             empty))
                                 false))
              (make-window (make-cell 33 21)
                           (cons (make-cell 30 40)
                                 (cons (make-cell 52 30)
                                       empty))
                           true))

;(define (stop w) w) ;stub

(define (stop w)
  (make-window (window-cursor w)
               (window-picture w)
               true))

;; Window -> Window
;; move cursor right
(check-expect (move-right (make-window (make-cell 33 12)
                                       empty
                                       false))
              (make-window (make-cell 34 12)
                           empty
                           false))
(check-expect (move-right (make-window (make-cell 32 52)
                                       (cons (make-cell 30 40)
                                             empty)
                                       false))
              (make-window (make-cell 33 52)
                           (cons (make-cell 30 40)
                                 empty)
                           false))
(check-expect (move-right (make-window (make-cell 33 21)
                                       (cons (make-cell 30 40)
                                             (cons (make-cell 52 30)
                                                   empty))
                                       false))
              (make-window (make-cell 34 21)
                           (cons (make-cell 30 40)
                                 (cons (make-cell 52 30)
                                       empty))
                           false))

;(define (move-right w) w) ;stub

;<Template from Window>
(define (move-right w)
  (make-window (cell-right (window-cursor w))
               (window-picture w)
               (window-pause? w)))

;; Window -> Window
;; move cursor left
(check-expect (move-left (make-window (make-cell 33 12)
                                      empty
                                      false))
              (make-window (make-cell 32 12)
                           empty
                           false))
(check-expect (move-left (make-window (make-cell 32 52)
                                      (cons (make-cell 30 40)
                                            empty)
                                      false))
              (make-window (make-cell 31 52)
                           (cons (make-cell 30 40)
                                 empty)
                           false))
(check-expect (move-left (make-window (make-cell 33 21)
                                      (cons (make-cell 30 40)
                                            (cons (make-cell 52 30)
                                                  empty))
                                      false))
              (make-window (make-cell 32 21)
                           (cons (make-cell 30 40)
                                 (cons (make-cell 52 30)
                                       empty))
                           false))

;<Template from Window>
(define (move-left w)
  (make-window (cell-left (window-cursor w))
               (window-picture w)
               (window-pause? w)))

;; Window -> Window
;; move cursor down
(check-expect (move-down (make-window (make-cell 33 12)
                                      empty
                                      false))
              (make-window (make-cell 33 13)
                           empty
                           false))
(check-expect (move-down (make-window (make-cell 32 52)
                                      (cons (make-cell 30 40)
                                            empty)
                                      false))
              (make-window (make-cell 32 53)
                           (cons (make-cell 30 40)
                                 empty)
                           false))
(check-expect (move-down (make-window (make-cell 33 21)
                                      (cons (make-cell 30 40)
                                            (cons (make-cell 52 30)
                                                  empty))
                                      false))
              (make-window (make-cell 33 22)
                           (cons (make-cell 30 40)
                                 (cons (make-cell 52 30)
                                       empty))
                           false))

;<Template from Window>
(define (move-down w)
  (make-window (cell-down (window-cursor w))
               (window-picture w)
               (window-pause? w)))

;; Window -> Window
;; move cursor up
(check-expect (move-up (make-window (make-cell 33 12)
                                    empty
                                    false))
              (make-window (make-cell 33 11)
                           empty
                           false))
(check-expect (move-up (make-window (make-cell 32 52)
                                    (cons (make-cell 30 40)
                                          empty)
                                    false))
              (make-window (make-cell 32 51)
                           (cons (make-cell 30 40)
                                 empty)
                           false))
(check-expect (move-up (make-window (make-cell 33 21)
                                    (cons (make-cell 30 40)
                                          (cons (make-cell 52 30)
                                                empty))
                                    false))
              (make-window (make-cell 33 20)
                           (cons (make-cell 30 40)
                                 (cons (make-cell 52 30)
                                       empty))
                           false))

;<Template from Window>
(define (move-up w)
  (make-window (cell-up (window-cursor w))
               (window-picture w)
               (window-pause? w)))



;; =============
;; Functions that come with cell

;; Cell -> Cell
;; move the given cell right
(check-expect (cell-right (make-cell 30 40))
              (make-cell 31 40))
(check-expect (cell-right (make-cell 99 32))
              (make-cell 100 32))

;(define (cell-right p) p) ;stub

;<Template from cell>
(define (cell-right p)
  (make-cell (add1 (cell-x p))
             (cell-y p)))

;; Cell -> Cell
;; move the given cell left
(check-expect (cell-left (make-cell 30 40))
              (make-cell 29 40))
(check-expect (cell-left (make-cell 99 32))
              (make-cell 98 32))

;(define (cell-left p) p) ;stub

;<Template from cell>
(define (cell-left p)
  (make-cell (sub1 (cell-x p))
             (cell-y p)))

;; Cell -> Cell
;; move the given cell up
(check-expect (cell-up (make-cell 30 40))
              (make-cell 30 39))
(check-expect (cell-up (make-cell 99 32))
              (make-cell 99 31))

;<Template from cell>
(define (cell-up p)
  (make-cell (cell-x p)
             (sub1 (cell-y p))))

;; Cell -> Cell
;; move the given cell down
(check-expect (cell-down (make-cell 30 40))
              (make-cell 30 41))
(check-expect (cell-down (make-cell 99 32))
              (make-cell 99 33))

;<Template from cell>
(define (cell-down p)
  (make-cell (cell-x p)
             (add1 (cell-y p))))

;; Cell Cell -> Boolean
;; pruduce true if two given cells are the same, false otherwise
(check-expect (cell=? (make-cell 1 1) (make-cell 1 1)) true)
(check-expect (cell=? (make-cell 22 32) (make-cell 22 32)) true)
(check-expect (cell=? (make-cell 43 23) (make-cell 43 23)) true)
(check-expect (cell=? (make-cell 1 1) (make-cell 32 43)) false)
(check-expect (cell=? (make-cell 21 32) (make-cell 1 1)) false)
(check-expect (cell=? (make-cell 21 43) (make-cell 93 75)) false)

;(define (cell=? p1 p2) false) ;stub

;<Template from cell expanded for two cells>
(define (cell=? p1 p2)
  (and (= (cell-x p1) (cell-x p2))
       (= (cell-y p1) (cell-y p2))))
#;
(local [(define (shh x) empty)]
  (time (shh (play-life (build-list 800 (lambda (x) (make-cell 40 x)))))))