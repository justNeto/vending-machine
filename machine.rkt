;;#!/usr/bin/racket -- linux only
#lang racket

(require "db/helper.rkt") ;; Creates the data and passes the correct values to this file

;;
;; machine.rkt runs helper.rkt and manages the logic after receiving the data that will be use for the machine
;;

(define update-inventory null)
(define update-deposit null)

; ;; TODO: create handlers for transaction file
; (define (start-transactions inventory transactions deposit)
;   transactions
; )

;; [start-transaction] of a single transaction file
(define (start-transaction inventory transaction)
  ;; get product in inventory and confirm it exist
  (cond
	[(product-exist? inventory transaction)"::--[Transacction completed]" ] ;; executes the transaction
	[else "::--[Transaction went wrong]"]
  )
)

;; helper function of [start-transaction]
;; if product-name exist then return true
(define (product-exist? inventory transaction)
  (cond
    ;; if inventory null then all inventory checked and product does not exist
    [(null? inventory) #f]
    [(and (equal? (caar inventory) (car transaction)) (compute-transaction (car inventory) transaction)) #t ]
    [else (product-exist? (cdr inventory) transaction) ]
    ; ;; (car transaction) = product name
    ; ;; (caar inventory = product name in first list of inventory)
    ; [(and (equal? (car transaction) (caar inventory)) (compute-transaction (car inventory) transaction)) #t ]
    ; [else (product-exist? (cdr inventory) transaction)]
    ; ;; if product in current index does not exist then pass the rest of inventory and pass the transaction
  )
)

;; [compute-transaction] validates the transaction and updates de db
(define (compute-transaction inventory transaction)
  ;; (car inventory) is the ("product-name" value quantity) that is being used for computing the transaction
  ;; (cadr transaction) is the list of symbols that will be validated
  ;; (cadr inventory) is the price of the product a.k.a the final state
  ;; validate transactions
  (cond
    [(and (validate (cadr transaction) (cadr inventory) 0) (check-currency (cadr transaction)) ) #t ]
    [else #f]
  )
  ;; set global variable to update inventory after validating transaction
  ;;(set! update-inventory (- (caddar inventory) 1))]
)

(define (validate transactions final-state current-state)
  (cond
    [(not(null? transactions)) (validate (cdr transactions) final-state (+ current-state (car transactions)) ) ]
    [(= final-state current-state) #t ] ;; validated without fare to return
    [(> current-state final-state) (change-automata current-state) ] ;; starts change automata
    [(< current-state final-state) #f ] ;; update the money added in the money db
  )
  ; (set! update-deposit ()) ;; set global variable to update deposit after validating transaction
)

(define (check-currency transactions)
  (cond
    [(null? transactions) #t]
    [(= (car transactions) 1) (check-currency (cdr transactions))  ]
    [(= (car transactions) 2) (check-currency (cdr transactions))  ]
    [(= (car transactions) 5) (check-currency (cdr transactions))  ]
    [(= (car transactions) 10) (check-currency (cdr transactions)) ]
    [(= (car transactions) 20) (check-currency (cdr transactions)) ]
    [(= (car transactions) 50) (check-currency (cdr transactions)) ]
    [else #f]
  )
)


(define (change-automata current-state)
  current-state
)

;; start of the runtime code
;; HERE THE PROGRAM STARTS

transaction
(cln)
inventory
(cln)

; (start-transaction inventory transaction)
; (validate (cadar transaction) (cadr inventory) 0)
