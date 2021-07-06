;Name: Mario Luja
;Date: 4/21/2021
;Program 12A
;The purpose of this program is to understand how matrices work
; and how to manipulate and traverse matrices.
;Function #1: Get the value in the cell that is indicated
(define (getCell Matrix row col)
  (if (= row 1)
      (traverseCol (car Matrix) col)
      (getCell (cdr Matrix ) (- row 1) col)
  )
)
;traverseCol helper function: Traverses column of matrix
(define (traverseCol lst col)
  (if ( = col 1)
      (car lst)
      (traverseCol (cdr lst) (- col 1))
  )

)
;Function #2: Replacing cell of matrix with an item
(define (setCell Matrix row col item)
  (if ( = row 1)
      (cons (placeNum (car Matrix) col item) (cdr Matrix))
      (if(null? Matrix)
         '()
         (cons (car Matrix)(setCell (cdr Matrix) (- row 1) col item))
      )
  )
 
)
;placeNum helper function: Places the item in a column
(define (placeNum lst col item)
  (if ( = col 1)
      (cons item (cdr lst))
      (if(null? lst)
         '()
         (cons (car lst)(placeNum (cdr lst) (- col 1) item))
      )
  )
)

  