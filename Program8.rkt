;Program 8
;Name: Mario Luja
;Date: 4/3/2021
;Description: To know how to use and traverse lists
;in scheme.

;Part a dot product
(define (dotProduct LST1 LST2)
  ;Check if both lists are empty
  (if (null? LST1)
      0         
      (+ (* (car LST1) (car LST2)) (dotProduct (cdr LST1) (cdr LST2)))
   )
)

;Part b: Check for duplicates
(define (duplicateList LST )
   (if (null? LST)
       #f
       (if (null? (cdr LST))
          #f
              (if ((findDup (cdr LST) (car LST)))
                  #t
                  (duplicateList (cdr LST))
              )
          
       )
    )
)
;traverse the list to find the duplicate
(define (findDup LST listMember)
   (if (null? LST)
      #f
       (if (= (car LST) listMember)
           #t
           (findDup (cdr LST) listMember)
        )
       
    )
)
;Part c: find the min product of two lists
(define (minProduct LST1 LST2)
   (if (null? LST1)
       '()
       (if(null? (cdr LST1))
          (* (car LST1) (car LST2))
          (if (findMin (*(car LST1) (car LST2)) (cdr LST1) (cdr LST2))
              (*(car LST1) (car LST2))
              (minProduct (cdr LST1) (cdr LST2))
          )
       )
    )
)


;find the minimum product while traversing list
(define (findMin product LST1 LST2)
   (if (null? LST1)
       #t
       (if (> product (*(car LST1) (car LST2)))
           #f
           (findMin product (cdr LST1) (cdr LST2))
       )
       
    )
)


  