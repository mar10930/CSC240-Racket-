;Name: Mario Luja
;Date: 3/3/2021
;Program 6
;The purpose of this program is to understand how to
; implement recursive functions in scheme to get
;solutions with less code.

;Part a: geometric progression recursively
(define (geometricProg scalar ratio n)
  (if (> n 1)
      (* (geometricProg scalar ratio (- n 1)) ratio)
      scalar
   )
)

;Part b: Power function

(define (powerFunction base exponent)

  (if (= exponent 0)
      1
  
      (if (even? exponent)
          (expt (powerFunction base (/ exponent 2)) 2)
          (* base (expt (powerFunction base (/ (- exponent 1) 2)) 2))
          )
   )
)

      
      
  