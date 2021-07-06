;Name: Mario Luja
;Date: 4/12/2021
;Description: The purpose of this progam is to implement bags as dotted
;pairs in scheme and to perform several tasks to the bags, most through
;returning new bags that are created using cons.

;getBagCount '{("a" . 3) ("b" . 2) ("c" . 1)}
(define (getBagCount List Item)
   (if(null? List)
      '()
       (if (string=? (car(car List)) Item)
           (cdr(car List))
           (getBagCount (cdr List) Item)
        )
    )
)

;insertBag returns a new bag with the new item added
(define (insertBag List Item)
  (if (null? List)
     (cons (cons Item 1) List)
        (if (string=? (car(car List)) Item)
           (cons (cons Item(+ (cdr (car List))1)) (cdr List))
           
           (cons (cons (car(car List)) (cdr(car List)))
           (insertBag (cdr List) Item))
         
        )
   )
)

;deleteBag returns a new bag with the new item added
(define (deleteBag List Item)
  (if (null? List)
     '{}
     (if (and (string=? (car(car List)) Item) (= (cdr(car List)) 1))
         (cdr List)
         (if (string=? (car(car List)) Item)
             (cons (cons Item(- (cdr (car List))1)) (cdr List))
   
             (cons (cons (car(car List)) (cdr(car List)))
             (deleteBag (cdr List) Item))
          ) 
      )
  )
)

;deleteAllBagList deletes all instances of item in bag
(define (deleteAllBag List Item)
  (if (null? List)
     '{}
     (if (string=? (car(car List)) Item)
         (cdr List)
         (cons (cons (car(car List)) (cdr(car List)))
         (deleteAllBag (cdr List) Item)) 
      )
  )
)
;unionBag adds all elements       
(define (unionBag ListA ListB)
  (if (and (null? ListA) (null? ListB))
     '{}
     (if (null? ListA)
         ListB
         (if (null? ListB)
             ListA
             (if (string=? (car(car ListA)) (car(car ListB)))
                (cons ( cons (car(car ListA)) (+ (cdr(car ListA)) (cdr(car ListB)))) 
                (unionBag (cdr ListA) (cdr ListB)))
               )
             )
         )
     )
  )
;IntersectBag 
(define (intersectBag ListA ListB)            
   (if (and (null? ListA) (null? ListB))
     '{}
     (if (null? ListA)
         ListB
         (if (null? ListB)
             ListA
             (if (string=? (car(car ListA)) (car(car ListB)))
                 (if (< (cdr(car ListA)) (cdr(car ListB)))
                    (cons ( cons (car(car ListA)) (cdr(car ListA)))    
                    (intersectBag (cdr ListA) (cdr ListB)))
                    (cons ( cons (car(car ListB)) (cdr(car ListB)))    
                    (intersectBag (cdr ListA) (cdr ListB)))
                 )
              )
          )
      )
  )
)
               
                     

  
          