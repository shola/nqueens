;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname nqueens_proper) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require racket/list) ; gets list-ref, take, drop

;; To run program: (print-bd (queens BD0))

;; PROBLEM STATEMENT:
;; The N-queens problem consists of finding a way to place N chess queens on a N by N chess board while making sure that none of the queens attack each other. 

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
(define SIZE 8)    ; can be adjusted to any Natural, however ROWS, COLS, and DIAGS8 
(define _ false)   ;   must be altered
(define Q true)
(define MAX-POS (sub1 (* SIZE SIZE))) ; position of last cell in board

(define BD0
  (list _ _ _ _ _ _ _ _
        _ _ _ _ _ _ _ _
        _ _ _ _ _ _ _ _
        _ _ _ _ _ _ _ _
        _ _ _ _ _ _ _ _
        _ _ _ _ _ _ _ _
        _ _ _ _ _ _ _ _
        _ _ _ _ _ _ _ _))

(define BD0s
  (list Q _ _ _ _ _ _ _
        _ _ _ _ Q _ _ _
        _ _ _ _ _ _ _ Q
        _ _ _ _ _ Q _ _
        _ _ Q _ _ _ _ _
        _ _ _ _ _ _ Q _
        _ Q _ _ _ _ _ _
        _ _ _ Q _ _ _ _))

(define BD1
  (list _ Q _ _ _ _ _ _
        _ _ _ _ _ _ _ _
        _ _ _ _ _ _ _ _
        _ _ _ _ _ _ _ _
        _ _ _ _ _ _ _ _
        _ _ _ _ _ _ _ _
        _ _ _ _ _ _ _ _
        _ _ _ _ _ _ _ _))

(define BD2
  (list _ _ Q _ _ _ _ _ 
        _ _ _ _ _ _ _ _
        _ _ _ _ _ _ _ _
        _ _ _ _ _ _ _ _
        _ _ _ _ _ _ _ _
        _ _ _ _ _ _ _ _
        _ _ _ _ _ _ _ _
        _ _ _ _ _ _ _ _))

(define BD3
  (list Q _ _ _ _ _ _ _ 
        _ _ Q _ _ _ _ _
        _ _ _ _ Q _ _ _
        _ _ _ _ _ _ Q _
        _ _ _ _ _ _ _ _
        _ _ _ _ _ _ _ _
        _ _ _ _ _ _ _ _
        _ _ _ _ _ _ _ _))

(define BD4
  (list Q _ _ _ _ _ _ _ 
        _ _ Q _ _ _ _ _
        _ Q _ _ Q _ _ _
        _ _ _ _ _ _ Q _
        _ _ _ _ _ _ _ _
        _ _ _ _ _ _ _ _
        _ _ _ _ _ _ _ _
        _ _ _ _ _ _ _ _))


;; Positions of all rows, columns, diagonals:

(define ROWS
  (list 
   (range 0 8)
   (range 8 16)
   (range 16 24)
   (range 24 32)
   (range 32 40)
   (range 40 48)
   (range 48 56)
   (range 56 64)))

(define COLS
  (list 
   (range 0 57 SIZE)
   (range 1 58 SIZE)
   (range 2 59 SIZE)
   (range 3 60 SIZE)
   (range 4 61 SIZE)
   (range 5 62 SIZE)
   (range 6 63 SIZE)
   (range 7 64 SIZE)))

(define DIAGS8
  (list
   (range 1)
   (range 8 0 (- 1 SIZE))
   (range 16 0 (- 1 SIZE))
   (range 24 0 (- 1 SIZE))
   (range 32 0 (- 1 SIZE))
   (range 40 0 (- 1 SIZE))
   (range 48 0 (- 1 SIZE))
   (range 56 0 (- 1 SIZE))
   (range 57 14 (- 1 SIZE))
   (range 58 22 (- 1 SIZE))
   (range 59 30 (- 1 SIZE))
   (range 60 38 (- 1 SIZE))
   (range 61 46 (- 1 SIZE))
   (range 62 54 (- 1 SIZE))
   (range 63 62 (- 1 SIZE))
   (range 7 0 (- (+ 1 SIZE)))
   (range 15 0 (- (+ 1 SIZE)))
   (range 23 0 (- (+ 1 SIZE)))
   (range 31 0 (- (+ 1 SIZE)))
   (range 39 0 (- (+ 1 SIZE)))
   (range 47 0 (- (+ 1 SIZE)))
   (range 55 0 (- (+ 1 SIZE)))
   (range 63 -1 (- (+ 1 SIZE)))
   (range 62 7 (- (+ 1 SIZE)))
   (range 61 15 (- (+ 1 SIZE)))
   (range 60 23 (- (+ 1 SIZE)))
   (range 59 31 (- (+ 1 SIZE)))
   (range 58 39 (- (+ 1 SIZE)))
   (range 57 47 (- (+ 1 SIZE)))
   (range 56 55 (- (+ 1 SIZE)))))

;;============
;; FUNCTIONS

;; Board -> Board|False
;; takes in a board and returns the first solved board if possible; false otherwise
;; ASSUME all boards begin as valid
(check-expect (queens BD3) false)
(check-expect (queens BD0) BD0s)

;(define (queens bd) false) ;stub

(define (queens bd)
  (local [(define all-solns (list))
          
          (define (solve-bd bd)         ; Board -> Board
            (cond [(solved? bd) bd]
                  [else (solve-lobd (gen-boards bd))]))
          
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
(check-expect (solved? BD0) false)
(check-expect (solved? BD0s) true)

;(define (solved? bd) false) ;stub
(define (solved? bd)
  (n-queens? bd))


;; Board -> Boolean
;; returns true if there are SIZE number of queens on board
(check-expect (n-queens? BD0s) true)
(check-expect (n-queens? BD3) false)

;(define (n-queens? bd) false) ;stub
(define (n-queens? bd)
  (= SIZE (length (filter identity bd))))


;; Board -> (listof Pos)
;; returns the pos of all queens on board
(check-expect (queens-pos BD0) empty)
(check-expect (queens-pos BD3) (list 0 10 20 30))

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
(check-expect (gen-boards BD0s) empty)
(check-expect (gen-boards (list Q _ _ _ _ _ _ _ 
                                _ _ Q _ _ _ _ _
                                _ _ _ _ Q _ _ _
                                _ _ _ _ _ _ Q _
                                _ Q _ _ _ _ _ _
                                _ _ _ Q _ _ _ _
                                _ _ _ _ _ _ _ _
                                _ _ _ _ _ _ _ _))
              (list
               (list Q _ _ _ _ _ _ _ 
                     _ _ Q _ _ _ _ _
                     _ _ _ _ Q _ _ _
                     _ _ _ _ _ _ Q _
                     _ Q _ _ _ _ _ _
                     _ _ _ Q _ _ _ _
                     _ _ _ _ _ Q _ _
                     _ _ _ _ _ _ _ _)))


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
(check-expect (valid-board? BD0) true)
(check-expect (valid-board? BD0s) true)
(check-expect (valid-board? BD3) true)
(check-expect (valid-board? BD4) false)
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
(check-expect (ins-pt BD0) 0)
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
    (remove-duplicates (flatten (fn DIAGS8)))))

;; Board Pos -> Board
;; produce a new board with Q at given position
(define (fill-square bd p)
  (append (take bd p)
          (list Q)
          (drop bd (add1 p))))

;; Board Pos -> Val or false
;; Produce value at given position on board.
(check-expect (read-square BD4 (r-c->pos 1 2)) true)
(check-expect (read-square BD0 (r-c->pos 6 3)) false)

(define (read-square bd p)
  (list-ref bd p))  

; Put board into (listof board), separeted into rows
(define (prettify bd) 
  (if (empty? bd)
      empty
      (cons (take bd SIZE)
            (prettify (drop bd SIZE)))))

; Converts board to rows, and each cell to appropriate 'Q and '_
(define (print-bd bd)
  (local [(define pretty-bd (prettify bd))
          
          (define (print-q cell)        ;; Boolean -> Symbol
            (if (false? cell)
                '_
                'Q))
          
          (define (print-ln pretty-ln)  ;; Board -> Board
            (map print-q pretty-ln))]
    
    (map print-ln pretty-bd)))


;;==============
;;Example Run:
(print-bd (queens BD0))
(print-bd (queens BD1))
(print-bd (queens BD2))