;Name: Mario Luja
;Program 4
;Date: February 19,2021
;Description: A scheme program that does calculations
; each through the use of a function. It also uses
; decisions like if and cond.

;Number 1
(define (WaterWeight diameter length waterType)
  (if waterType
     (* (* (* 3.14159 (/ diameter 24)) length) 62.4)
     (* (* (* 3.14159 (/ diameter 24)) length) 64.08)
  )
)

;Number2
(define (FrequencyBand frequency)
  (if (>= frequency 0.001);Lowest value in khz (10kHz)
     (if (<= frequency 0.03);Check if frequency <= 30kHz
           "VLF"
        (if (<= frequency 0.3);Check if frequency <= 300kHz
              "LF"
           (if (<= frequency 3);Check if frequency <= 3MHz
               "MF"
               (if (<= frequency 30);Check if frequency <= 30MHz
                   "HF"
                   (if (<= frequency 328.6);Check if frequency <= 328.6MHz
                       "VHF"
                       (if (<= frequency 2009);Check if frequency <= 2009MHz
                           "UHF"
                          
                        )
                    )
                )
            )
        )
     )
  )
)

;Number 3
(define (WeeklyPaycheck employeeType hoursOrSales rate giftFund)
  ;Check employee type, hourly first
  (if employeeType  
      (if (> hoursOrSales 40) 
          (- (+ (* rate 40) (* rate (* (- hoursOrSales 40) 1.5))) giftFund)
          (- (* rate hoursOrSales) giftFund)
       )
      ;Commisioned employees
      (if (< hoursOrSales 500)
          (- (* hoursOrSales 0.015) giftFund)
          (if (>= hoursOrSales 500)
              (if(< hoursOrSales 1000)
                 (- (* hoursOrSales 0.03) giftFund)
                 (if (>= hoursOrSales 1000)
                     (- (* hoursOrSales 0.05) giftFund)
                  )
              )
           )
       )
   )          
)
