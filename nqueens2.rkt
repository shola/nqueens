;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname nqueens2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require racket/list) ; gets list-ref, take, drop

;; To run program: (prettify (queens BD1))

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

(define BD1s
  (list _ Q _ _
        _ _ _ Q
        Q _ _ _
        _ _ Q _))

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

(define BD5
  (list Q _ _ _
        _ _ _ _
        _ _ Q _
        _ _ _ _))

(define BD6
  (list _ _ _ _
        _ _ _ _
        _ _ _ Q
        _ _ Q _))

;; Positions of all rows, columns, diagonals:

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

(define DIAGS2
  (list (list 0)
        (list 1 4)
        (list 2 5 8)
        (list 3 6 9 12)
        (list 7 10 13)
        (list 11 14)
        (list 15)
        (list 3)
        (list 2 7)
        (list 1 6 11)
        (list 0 5 10 15)
        (list 4 9 14)
        (list 8 13)
        (list 12)))

;;============
;; FUNCTIONS

;; Board -> Board|False
;; takes in a board and returns a solved board if possible; false otherwise
;; ASSUME all boards begin as valid
(check-expect (queens BD1) BD1s)
(check-expect (queens BD4) false)
(check-expect (queens BD2) BD2)

;(define (queens bd) false) ;stub

(define (queens bd)
  (local [(define (solve-bd bd)         ; Board -> Board
            (if (solved? bd)
                bd
                (solve-lobd (gen-boards bd))))
          
          (define (solve-lobd lobd)     ; (listof Board) -> Boolean
            (cond [(empty? lobd) false]
                  [else 
                   (local [(define try (solve-bd (first lobd)))]
                     (if (not (false? try))
                         try
                         (solve-lobd (rest lobd))))]))]
    
    (solve-bd bd)))


;; Board -> Boolean
;; returns true if the board is a valid solution (with max num queens), 
;; false otherwise
;; ASSUME: only valid boards are passed to solve-bd
(check-expect (solved? BD1) false)
(check-expect (solved? BD4) false)
(check-expect (solved? BD2) true)

;(define (solved? bd) false) ;stub
(define (solved? bd)
  (n-queens? bd))


;; Board -> Boolean
;; returns true if there are SIZE number of queens on board
(check-expect (n-queens? BD1) false)
(check-expect (n-queens? BD2) true)
(check-expect (n-queens? BD4) false)

;(define (n-queens? bd) false) ;stub
(define (n-queens? bd)
  (= SIZE (length (filter identity bd))))


;; Board -> (listof Pos)
;; returns the pos of all queens on board
(check-expect (queens-pos BD1) empty)
(check-expect (queens-pos BD2) (list 2 4 11 13))

(define (queens-pos bd)
  (local [(define (add-qs b p)        ; Board Pos -> (listof Pos)
            (cond [(empty? b) empty]
                  [(first b) 
                   (cons p (add-qs (rest b) (add1 p)))]
                  [else (add-qs (rest b) (add1 p))]))]
    (add-qs bd 0)))

;; Board -> (listof Board)
;; takes current board, and generates a list of valid boards with a queen 
;; in each cell after the last queen's cell
;; ASSUME: number of queens on parameter bd will be less than size
(check-expect (gen-boards BD4) empty)
(check-expect (gen-boards (list Q _ _ _
                                _ _ _ _
                                _ _ _ _
                                _ _ _ Q))
              empty)
(check-expect (gen-boards (list _ _ _ _
                                _ _ _ _
                                _ Q _ _
                                _ _ _ _)) 
              (list 
               (list _ _ _ _
                     _ _ _ _
                     _ Q _ _
                     _ _ _ Q)))

;(define (gen-boards bd) empty) ;stub
(define (gen-boards bd)
  (local [(define start-pos (ins-pt bd))
          (define (add-boards p)      ; Pos -> (listof Board)
            (cond [(> p MAX-POS) empty]
                  [else (cons (fill-square bd p)
                              (add-boards (add1 p)))]))]
    (filter valid-board? (add-boards start-pos))))


;; Board -> Boolean
;; returns true if no queens attack each other
;; ASSUME: that there are SIZE or less queens
(check-expect (valid-board? BD2) true)
(check-expect (valid-board? BD1) true)
(check-expect (valid-board? BD3) false)
(check-expect (valid-board? BD6) false)
;(define (valid-board? bd) false) ;stub

(define (valid-board? bd) 
  (local [(define q-pos (queens-pos bd))
          
          (define rows (map pos->r q-pos))
          
          (define cols (map pos->c q-pos))
          
          (define diag (map pos->d2 q-pos))
          
          (define (row-attack? r) (unique? r))  ; (listof Pos) -> Boolean
          
          (define (col-attack? c) (unique? c))] ; (listof Pos) -> Boolean
    
    (and (row-attack? rows)
         (and (col-attack? cols)
              (not (diag-attack? diag q-pos))))))

;; (listof X) -> Boolean
;; returns true if there are no duplicates in the list
(check-expect (unique? '(1 2 3 4 5)) true)
(check-expect (unique? '(1 5 2 3 4 5)) false)

(define (unique? lox)
  (local [(define (check-neighbors lox)    ; (listof X) -> Boolean
            (cond [(or (empty? lox) (empty? (rest lox))) true]
                  [(= (first lox) (second lox)) false]
                  [else (check-neighbors (rest lox))]))]
    
    (check-neighbors (sort lox <))))


;; (listof (listof Pos)) (listof Pos) -> Boolean
;; takes in a list of diagonal-pos-lists, and a list of queen-pos.
;;  if 2 or more queen-pos items are in a given diagonal-pos-list, 
;;  then the queens are attacking each other (return true).
(check-expect (diag-attack? (list (list 0 5 10 15) (list 7 10 13 0 5 15))
                            (list 0 10))
              true)
(check-expect (diag-attack? (list (list 0 5 10 15) (list 11 14 1 6) (list 7 10 13 8))
                            (list 0 11 13))
              false)

;(define (diag-attack? lod loq) false)

(define (diag-attack? lod loq) 
  (local [(define (check-diag diag queens)    ;; (listof Pos) (listof Pos) -> (listof Bool)
            (cond [(empty? queens) empty]
                  [(member (first queens) diag) 
                   (cons Q (check-diag diag (rest queens)))]
                  [else (check-diag diag (rest queens))]))
          
          (define (check-all diags)          ;; (listof Pos) -> Boolean
            (cond [(empty? diags) false]
                  [(>= (length (check-diag (first diags) loq)) 2)
                   true]
                  [else (check-all (rest diags))]))]
    
    (check-all lod)))


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
  (local [(define loq (queens-pos bd))]
    (if (empty? loq)
        0
        (add1 (first (reverse loq))))))


;; UTILITY FUNCTIONS

;; Helper to convert 0-based row and col to Pos
(define (r-c->pos r c) (+ (* r SIZE) c))

;; Helper to convert Pos to 0-based row
(define (pos->r p) (quotient p SIZE))

;; Helper to convert Pos to 0-based col
(define (pos->c p) (remainder p SIZE))

;; Helper to convert Pos to list of pos on diagonals
(define (pos->d2 p) 
  (local [(define (fn lod)
            (cond [(empty? lod) empty] 
                  [(member p (first lod))
                   (cons (first lod) (fn (rest lod)))]
                  [else (fn (rest lod))]))]
    (remove-duplicates (flatten (fn DIAGS2)))))

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


;;==============
;;Example Run:
(prettify (queens BD1))