;Name: Mario Luja
;Date: May 7,2021
;Description: The purpose of this program is to implement
;all the techniques we have learned in scheme over the semester
;into a project that relies heavily on matrices and recurssion.
;Part A
(define MALGame 0)
(define (MALStartGame)
  (begin
    (set! MALGame '(1 (0 0 0 0 0 0 0) (0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0) (0 0 0 0 0 0 0) (0 0 0 0 0 0 0) (0 0 0 0 0 0 0)))
    (display "Eureka!!!! The board is ALIVE!!!") (newline)
    #t
  )
)
;Part B
(define (MALMarkMove col)
  (begin
       (set! MALGame(cons (MALNextPlayer) (MALplayMove (cdr MALGame) col (car MALGame))))
       col
  )
)
(define (MALplayMove Matrix col item)
  (if (null? (cdr Matrix))
        (cons (MALplaceNum (car Matrix) col item)'())
    (if (or (=(MALTraverseCol (car(cdr Matrix)) col) 1)
            (=(MALTraverseCol (car(cdr Matrix)) col) 2))
        (cons (MALplaceNum (car Matrix) col item) (cdr Matrix))
        (cons (car Matrix)(MALplayMove (cdr Matrix) col item))
    )
  )
)

;placeNum helper function: Places the item in a column
(define (MALplaceNum lst col item)
  (if ( = col 1)
      (cons item (cdr lst))
      (if(null? lst)
         '()
         (cons (car lst)(MALplaceNum (cdr lst) (- col 1) item))
      )
  )
)

(define (MALTraverseCol lst col)
  (if ( = col 1)
      (car lst)
      (MALTraverseCol (cdr lst) (- col 1))
  )

)
(define (MALNextPlayer)
  (if (= (car MALGame) 1)
      2
      1
  )
)
 
;Part C
(define (MALShowGame)
  (begin
    (MALShowPlayer)(newline)
    (display (car(cdr MALGame))) (newline)
    (MALTraverseList (cdr(cdr MALGame)))
    #t
  )
)
(define (MALShowPlayer)
  (if (= (car MALGame) 1)
     (display "Player 1")
     (display "Player 2")
  )
)
(define (MALTraverseList lst)
  (display (car lst))
  (newline)
  (if (null? (cdr lst))
      '()
      (MALTraverseList (cdr lst))
  ) 
)

;Part D
(define (MALMakeMove)
  (MALNextPlayer)
  (if (= (MALCPUWin 1) 0)
      ( MALMarkMove( + 1 (random 7)))
      ( MALMarkMove((MALCPUWin 1))) 
  )
)
;AI Implementation
(define (MALCPUWin col)
  (if ( = col 8)
      0
      (if(MALWillWinP col)
         col
         (MALCPUWin (+ col 1))
       )
   )
)
  

;Part E
(define (MALLegalMoveP col)
  (if (or (= (MALGetCell (cdr MALGame) 6 col) 1)
          (=(MALGetCell (cdr MALGame) 6 col) 2))
      #f
      #t
   )
)


(define (MALGetCell Matrix row col)
  (if (= row 1)
      (MALTraverseCol (car Matrix) col)
      (MALGetCell (cdr Matrix ) (- row 1) col)
  )
)

;Part F
(define (MALWinP lastCol)
  (if (MALVerticalWin1(cdr MALGame) lastCol 4)
      #t
      (if (MALVerticalWin2(cdr MALGame) lastCol 4)
          #t
          (if(MALHorizontalWin1 (cdr MALGame) lastCol )
             #t
             (if(MALHorizontalWin2 (cdr MALGame) lastCol)
                #t
                (if (MALDiagonal1 (cdr MALGame) lastCol)
                    #t
                    (if (MALDiagonal2 (cdr MALGame) lastCol)
                       #t
                       #f
                   )
                )
             )
          )
       )
   )
)

(define (MALVerticalWin1 matrix col num)
    (if (= num 0)
        #t
       (if (null? matrix)
           #f
           (if ( = (MALTraverseCol (car matrix) col)1)
               (MALVerticalWin1 (cdr matrix) col (- num 1))
               (MALVerticalWin1 (cdr matrix) col 4)
            )
       )
   )
)
(define (MALVerticalWin2 matrix col num)
    (if (= num 0)
        #t
       (if (null? matrix)
           #f
           (if ( = (MALTraverseCol (car matrix) col)2)
               (MALVerticalWin2 (cdr matrix) col (- num 1))
               (MALVerticalWin2 (cdr matrix) col 4)
            )
       )
   )
)

(define (MALHorizontalWin1 matrix col )
  (if (null? matrix)
      #f
     (if ( = (MALTraverseCol (car matrix) col)1)
         (MALListElements1 (car matrix) 4)
         (MALHorizontalWin1 (cdr matrix) col)
     )
  )
)
      
(define (MALHorizontalWin2 matrix col )
  (if (null? matrix)
      #f
     (if ( = (MALTraverseCol (car matrix) col)1)
         (MALListElements2 (car matrix) 4)
         (MALHorizontalWin2 (cdr matrix) col)
     )
  )
)     


(define (MALListElements1 lst num)
  (if (= num 0)
      #t
     (if (null? (cdr lst))
         #f
         (if (= (car lst) 1) 
           (MALListElements1 (cdr lst) (- num 1))
           (MALListElements1 (cdr lst) 4)
         )
     )
  )
)
(define (MALListElements2 lst num)
  (if (= num 0)
      #t
     (if (null? (cdr lst))
         #f
         (if (= (car lst) 2) 
           (MALListElements2 (cdr lst) (- num 1))
           (MALListElements2 (cdr lst) 4)
         )
     )
  )
)

;Diagonal win functions

(define (MALDiagonalBottomLeft1 matrix row col num)
  (if (= row 7)
       ( + num 0)
      (if ( = col 0)
          (+ num 0)
          (if( = (MALGetCell matrix row col) 1)
             (MALDiagonalBottomLeft1 matrix (+ row 1) (- col 1) (+ num 1))
             (+ num 0)
           )
      )
  )
)

(define (MALDiagonalBottomRight1 matrix row col num)
  (if (= row 7)
       ( + num 0)
      (if ( = col 7)
          (+ num 0)
          (if( = (MALGetCell matrix row col) 1)
             (MALDiagonalBottomRight1 matrix (+ row 1) (+ col 1) (+ num 1))
             (+ num 0)
           )
      )
  )
)
(define (MALDiagonalTopLeft1 matrix row col num)
  (if (= row 0)
       ( + num 0)
      (if ( = col 0)
          (+ num 0)
          (if( = (MALGetCell matrix row col) 1)
             (MALDiagonalTopLeft1 matrix (- row 1) (- col 1) (+ num 1))
             (+ num 0)
           )
      )
  )
)
(define (MALDiagonalTopRight1 matrix row col num)
  (if (= row 0)
       ( + num 0)
      (if ( = col 7)
          (+ num 0)
          (if( = (MALGetCell matrix row col) 1)
             (MALDiagonalTopRight1 matrix (- row 1) (+ col 1) (+ num 1))
             (+ num 0)
           )
      )
  )
)

(define (MALDiagonalBottomLeft2 matrix row col num)
  (if (= row 7)
       ( + num 0)
      (if ( = col 0)
          (+ num 0)
          (if( = (MALGetCell matrix row col) 2)
             (MALDiagonalBottomLeft2 matrix (+ row 1) (- col 1) (+ num 1))
             (+ num 0)
           )
      )
  )
)

(define (MALDiagonalBottomRight2 matrix row col num)
  (if (= row 7)
       ( + num 0)
      (if ( = col 7)
          (+ num 0)
          (if( = (MALGetCell matrix row col) 2)
             (MALDiagonalBottomRight2 matrix (+ row 1) (+ col 1) (+ num 1))
             (+ num 0)
           )
      )
  )
)
(define (MALDiagonalTopLeft2 matrix row col num)
  (if (= row 0)
       ( + num 0)
      (if ( = col 0)
          (+ num 0)
          (if( = (MALGetCell matrix row col) 2)
             (MALDiagonalTopLeft2 matrix (- row 1) (- col 1) (+ num 1))
             (+ num 0)
           )
      )
  )
)
(define (MALDiagonalTopRight2 matrix row col num)
  (if (= row 0)
       ( + num 0)
      (if ( = col 7)
          (+ num 0)
          (if( = (MALGetCell matrix row col) 2)
             (MALDiagonalTopRight2 matrix (- row 1) (+ col 1) (+ num 1))
             (+ num 0)
           )
      )
  )
)
(define (MALgetRow matrix col rowNum)
  (if (null? (cdr matrix))
      6
      (if ( > (MALTraverseCol (car matrix) col) 0)
         rowNum
         (MALgetRow (cdr matrix) col (+ rowNum 1))
      )
  )
)

(define (MALDiagonal1 matrix col)
  (if(= (+ (MALDiagonalBottomLeft1 (cdr MALGame) (MALgetRow (cdr MALGame) col 1) col 0)
          (MALDiagonalTopLeft1 (cdr MALGame) (MALgetRow (cdr MALGame) col 1) col 0)) 4)
     #t
     (if(=(+ (MALDiagonalBottomRight1 (cdr MALGame)  (MALgetRow (cdr MALGame)col 1) col 0)
              (MALDiagonalTopRight1 (cdr MALGame) (MALgetRow (cdr MALGame)col 1) col 0)) 4)
        #t
        #f
     )
  )
)
(define (MALDiagonal2 matrix col)
  (if(= (+ (MALDiagonalBottomLeft2 (cdr MALGame) (MALgetRow (cdr MALGame) col 1) col 0)
          (MALDiagonalTopLeft2 (cdr MALGame) (MALgetRow (cdr MALGame) col 1) col 0)) 4)
     #t
     (if(= (+ (MALDiagonalBottomRight2 (cdr MALGame) (MALgetRow (cdr MALGame)col 1) col 0)
              (MALDiagonalTopRight2 (cdr MALGame)(MALgetRow (cdr MALGame)col 1) col 0)) 4)
        #t
        #f
     )
  )
)
;WillWin Function Part G
(define (MALWillWinP col)
  (checkFutureBoard
   (MALsetCell (cdr MALGame) (-(MALgetRow (cdr MALGame) col 1)1) col (car MALGame))
    col)
)
(define (MALsetCell Matrix row col item)
  (if ( = row 1)
      (cons (MALplaceNum (car Matrix) col item) (cdr Matrix))
      (if(null? Matrix)
         '()
         (cons (car Matrix)(MALsetCell (cdr Matrix) (- row 1) col item))
      )
  )
 
)
;CheckFutureBoard
(define (checkFutureBoard matrix lastCol)
  (if (MALVerticalWin1 matrix lastCol 4)
      #t
      (if (MALVerticalWin2 matrix lastCol 4)
          #t
          (if(MALHorizontalWin1 matrix lastCol )
             #t
             (if(MALHorizontalWin2 matrix lastCol)
                #t
                (if (MALDiagonal1 matrix lastCol)
                    #t
                    (if (MALDiagonal2 matrix lastCol)
                       #t
                       #f
                   )
                )
             )
          )
       )
   )
)