;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname nqueens) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require racket/list) ; gets list-ref, take, drop

;; PROBLEM STATEMENT:
;; The four queens problem consists of finding a way to place four chess queens on a 4 by 4 chess board while making sure that none of the queens attack each other. The four queens puzzle is one version of the more general n queens problem of placing n queens on an n by n board.

;; ================
;; DATA DEFINITIONS

;; Cell is Boolean

;; Board is (listof Cell) that is (* SIZE SIZE) elements long
;; interp. a board is a SIZExSIZE list of cells, where each cell can be
;;  thought of as having a row and col (see Pos below).

;; Pos is Natural[0, (* SIZE SIZE)]
;; interp. the position of a cell on the board, for a given p, then
;;   - the row is (quotient p SIZE)
;;   - the col is (remainder p SIZE)

;; =========
;; CONSTANTS
(define SIZE 4)
(define _ false)
(define Q true)
(define MAX-POS (sub1 (* SIZE SIZE))) ; position of last cell in board

(define BD1        ;blank board
  (list _ _ _ _
        _ _ _ _
        _ _ _ _
        _ _ _ _))

(define BD2        ;1 solution
  (list _ _ Q _
        Q _ _ _
        _ _ _ Q
        _ Q _ _))

(define BD3        ;not a solution
  (list Q _ Q _
        _ _ _ _
        _ _ _ Q
        _ Q _ _))

(define BD4        ;not a solution
  (list Q _ _ _
        _ _ _ _
        _ _ _ Q
        _ Q _ _))


;; Positions of all rows and columns:

(define ROWS
  (list (list 0  1  2  3)
        (list 4  5  6  7)
        (list 8  9  10 11)
        (list 12 13 14 15)))

(define COLS
  (list (list 0 4 8  12)
        (list 1 5 9  13)
        (list 2 6 10 14)
        (list 3 7 11 15)))

;;============
;; FUNCTIONS

;; Board -> Board|False
;; takes in a board and returns a solved board if possible; false otherwise
(check-expect (queens BD1) BD2)
(check-expect (queens BD4) false)
(check-expect (queens BD2) BD2)
(check-expect (queens BD3) false)

;(define (queens bd) false) ;stub

(define (queens bd)
  (local [(define (solve-bd bd)
            (if (solved? bd)
                bd
                (solve-lobd (gen-boards bd))))
          
          (define (solve-lobd lobd)
            (cond [(empty? lobd) false]
                  [else 
                   (local [(define try (solve-bd (first lobd)))]
                     (if (not (false? try))
                         try
                         (solve-lobd (rest lobd))))]))]
    (solve-bd bd)))


;; Board -> Boolean
;; returns true if the board is a valid solution, false otherwise
;; ASSUME: only valid boards are generated?
(check-expect (solved? BD1) false)
(check-expect (solved? BD4) false)
(check-expect (solved? BD2) true)

;(define (solved? bd) false) ;stub
(define (solved? bd)
  (and (n-queens? bd) (valid-board? bd)))

;; Board -> Boolean
;; returns true if there are SIZE number of queens on board
(check-expect (n-queens? BD1) false)
(check-expect (n-queens? BD2) true)
(check-expect (n-queens? BD4) false)

;(define (n-queens? bd) false) ;stub
(define (n-queens? bd)
  (= SIZE (length (filter identity bd))))


;; Board -> Boolean
;; returns true if no queens attack each other
;; no tests needed, since queens will only run if valid-board? passes
;; ASSUME: that there are SIZE or less queens
;(define (valid-board? bd) false) ;stub

(define (valid-board? bd)
  (local [(define q-pos (queens-pos bd))
          (define q-rows (map get-row q-pos))
          (define q-cols (map get-col q-pos))
          (define q-diag (map get-diag q-pos))
          (define (stepper index)
            )]
    (stepper 0)))

;; Board -> (listof Natural)
;; returns the pos of all queens on board
(check-expect (queens-pos BD1) empty)
(check-expect (queens-pos BD2) (list 2 4 11 13))

(define (queens-pos bd)
  (local [(define (add-qs b p)
            (cond [(empty? b) empty]
                  [(first b) 
                   (cons p (add-qs (rest b) (add1 p)))]
                  [else (add-qs (rest b) (add1 p))]))]
    (add-qs bd 0)))

;; Board -> (listof Board)
;; takes current board, and generates a list of valid boards with a queen in each cell after 
;; the last queen's cell
;; ASSUME: number of queens on bd will never be higher than 3
(check-expect (gen-boards (list Q _ _ _
                                _ _ _ _
                                _ _ _ _
                                _ _ _ Q))
              empty)
(check-expect (gen-boards BD4) (list 
                                (list Q _ _ _
                                      _ _ _ _
                                      _ _ _ Q
                                      _ Q Q _)
                                (list Q _ _ _
                                      _ _ _ _
                                      _ _ _ Q
                                      _ Q _ Q)))

;(define (gen-boards bd) empty) ;stub
(define (gen-boards bd)
  (local [(define start-pos (ins-pt bd))
          (define (add-boards p)
            (cond [(> p MAX-POS) empty]
                  [else (cons (fill-square bd p)
                              (add-boards (add1 p)))]))]
    (add-boards start-pos)))


;; Board -> Pos[0, (sub1 (* SIZE SIZE)) )
;; returns the position right after the queen closest to the end of the board
;; (AKA the insertion point)
;; NOTE: returns 0 if there is an empty board
(check-expect (ins-pt BD1) 0)
(check-expect (ins-pt (list _ _ _ _
                            _ _ Q _
                            _ _ _ _
                            _ _ _ _)) 
              7)
(check-expect (ins-pt (list _ _ _ _
                            _ _ _ _
                            _ _ _ _
                            _ _ Q _)) 
              15)

;(define (ins-pt bd) 0) ;stub
(define (ins-pt bd)
 (first (reverse (queens-pos bd))))
;  (local [(define (find-next-q b p)    ; Board Pos -> Pos
;            (cond [(empty? b) 0]
;                  [(first b) (add1 p)]
;                  [else (find-next-q (rest b) (sub1 p))]))]
;    (find-next-q (reverse bd) MAX-POS)))


;; Natural -> (listof Natural|false)
;; gets the positions of all squares in the same row as pos
(define (get-row pos)
  (first (drop ROWS (quotient pos SIZE))))

;; Natural -> (listof Natural|false)
;; gets the positions of all squares in the same col as pos
(define (get-col pos)
  (first (drop COLS (remainder pos SIZE))))

;; Natural -> (listof POS|false)
;; generates a list of the diagonal pos's, centered at (r, c).
;; NOTE: (append UL UR LL LR)
(check-expect (get-diag 0) (list 5 10 15))
(check-expect (get-diag 11) (list 6 1 14))
(check-expect (get-diag 5) (list 0 2 8 10 15))

;(define (get-diag p) empty) ;stub
(define (get-diag p)
  (local [(define r (pos->r p))
          (define c (pos->c p))
          (define (get-ul r c)
            (if (or (< r 0) (< c 0))
                empty
                (cons (r-c->pos r c) 
                      (get-ul (sub1 r) (sub1 c)))))
          (define (get-ur r c)
            (if (or (< r 0) (>= c SIZE))
                empty
                (cons (r-c->pos r c) 
                      (get-ur (sub1 r) (add1 c)))))
          (define (get-ll r c)
            (if (or (>= r SIZE) (< c 0))
                empty
                (cons (r-c->pos r c) 
                      (get-ll (add1 r) (sub1 c)))))
          (define (get-lr r c)
            (if (or (>= r SIZE) (>= c SIZE))
                empty
                (cons (r-c->pos r c) 
                      (get-lr (add1 r) (add1 c)))))]
    (append (get-ul (sub1 r) (sub1 c))
            (get-ur (sub1 r) (add1 c))
            (get-ll (add1 r) (sub1 c))
            (get-lr (add1 r) (add1 c)))))


;; UTILITY FUNCTIONS

;; Helper to convert 0-based row and col to Pos
(define (r-c->pos r c) (+ (* r SIZE) c))
;; Helper to convert Pos to 0-based row
(define (pos->r p) (quotient p SIZE))
;; Helper to convert Pos to 0-based col
(define (pos->c p) (remainder p SIZE))

;; Board Pos -> Board
;; produce a new board with Q at given position
(define (fill-square bd p)
  (append (take bd p)
          (list Q)
          (drop bd (add1 p))))

;; Board Pos -> Val or false
;; Produce value at given position on board.
(check-expect (read-square BD2 (r-c->pos 0 2)) true)
(check-expect (read-square BD3 (r-c->pos 2 2)) false)

(define (read-square bd p)
  (list-ref bd p))  

(define (prettify bd) 
  (if (empty? bd)
      empty
      (cons (take bd SIZE)
            (prettify (drop bd SIZE)))))

;; gets the positions on the diagonals of the current pos
;; NOTE 1: You can tell whether two queens are on the same diagonal by comparing the slope of the line between them. If one queen is at row and column (r1, c1) and another queen is at row and column (r2, c2) then the slope of the line between them is: (/ (- r2 r1) (- c2 c1)). If that slope is 1 or -1 then the queens are on the same diagonal.


;; Here's what you need to know about the board and queens to solve the four queens problem:

;; - The BOARD consists of 16 individual SQUARES arranged in 4 rows of 4 columns. The colour of the squares does not matter. Each square can either be empty or can contain a queen.
;; - A POSITION on the board refers to a specific square.
;; - A queen ATTACKS every square in its row, its column, and both of its diagonals.
;; - A board is VALID if none of the queens placed on it attack each other.
;; - A valid board is SOLVED if it contains 4 queens.

;; NOTE 1: You can tell whether two queens are on the same diagonal by comparing the slope of the line between them. If one queen is at row and column (r1, c1) and another queen is at row and column (r2, c2) then the slope of the line between them is: (/ (- r2 r1) (- c2 c1)). If that slope is 1 or -1 then the queens are on the same diagonal.
;;
;; There are many strategies for solving queens, but you should use the following:
;;
;; - Use a backtracking generative search that is trying to add 1 queen at a time to the board. So at each step you generate the next boards by finding all the empty squares, adding a queen to each empty square and discarding invalid boards. If the search reaches a valid board with 4 queens (a solved board) produce that result. So its A LOT like the Sudoku solver.
;; - You should design a function that consumes a board - which will initially be empty - and tries to find a solution. Call your function queens.
;;
;; NOTE 1: You can tell whether two queens are on the same diagonal by comparing the slope of the line between them. If one queen is at row and column (r1, c1) and another queen is at row and column (r2, c2) then the slope of the line between them is: (/ (- r2 r1) (- c2 c1)). If that slope is 1 or -1 then the queens are on the same diagonal.
;;
;; NOTE 2: We have described the problem as working for 4 queens. Feel free to make your design have a constant, SIZE, that controls how many queens it works for. You can also just make your function work for any board, but that will slightly complicate some of your functions.
;;
;; A more detailed rubric will be published when the self-assessment period begins, but suffice to say that it will be substantially similar to the rubric for Project 1. Significant weight will be put on following the recipes and producing a program that is easy to read. Less weight will be put on final correctness. Remember, a program that is partially complete and is easy to read is easy to get help with. But a program that almost works and is hard to read is hard to get help with!

