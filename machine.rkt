;;#!/usr/bin/racket -- linux only
#lang racket

(require "db/helper.rkt") ;; Creates the data and passes the correct values to this file

;;
;; machine.rkt runs helper.rkt and manages the logic after receiving the data that will be use for the machine
;; updates of the db's (inventory and money-deposit) are done by writing files in the system and then reading them.
;; in helper.rkt the logic for update and reading is implemented
;;

; ;; TODO: create handlers for transaction file
; (define (start-transactions inventory transactions deposit)
;   transactions
; )

; ;; [start-transaction] of a single transaction file
; (define (start-transaction inventory transaction)
;   ;; get product in inventory and confirm it exist
;   (cond [(product-exist? inventory transaction) "::--[Transacction completed]" ] ;; executes the transaction
; 	[else "::--[Transaction went wrong]"]
;   )
; )


;; [start-transaction] of a single transaction file
(define (start-transaction inventory transaction)
  ;; get product in inventory and confirm it exist
  (cond
	[(product-exist? inventory transaction) #t ] ;; executes the transaction
	[else #f]
  )
)

;; helper function of [start-transaction]
;; if product-name exist then return true
(define (product-exist? inventory transaction)
  (cond
    ;; if inventory null then all inventory checked, if compute-transaction is false then the transaction is not valid, so returns false
    [(null? inventory) #f]
    [(and (equal? (caar inventory) (car transaction)) (compute-transaction (car inventory) transaction)) #t ]
    [else (product-exist? (cdr inventory) transaction) ]
  )
)

;; [compute-transaction] validates the transaction and updates de db
;; Getting money in
(define (compute-transaction inventory transaction)
  ;; (cadr transaction) is the list of transitions form '(1 1 2 5 10 20 50)
  ;; (cadr inventory) is the price form '("product-name" price quantity) : ("gansitos" 17 2)

  (cond
    [(and (check-currency-in (cadr transaction)) (validate (cadr transaction) (cadr inventory) 0)) #t ]
    [else #f]
  )

)

;; HERE DATA SHOULD BE UPDATED
;; current-state = money deposited by user
;; final-state = money that user should deposit
(define (validate transactions final-state current-state)
  (cond
    ;; If transactions not empty yet, then continue to add up the transactions inside the function
    [(not(null? transactions)) (validate (cdr transactions) final-state (+ current-state (car transactions)) ) ]

    ;; [[[[[[[[[[[---------- Update happens here in function that returns true ----------]]]]]]]]
    ;; validated transactions without fare to return. update inv and money using transactions data
    [(= final-state current-state) #t ]

    ;; Change automata only needs to substract the expected value of the product to the value inputed by the user
    ;; For example, if a product costs 50 and user gave 55, then the machine must return 5 (input - cost)

    ;; [[[[[[[[[[[---------- Update happens here in function that returns true ----------]]]]]]]]
    [(> current-state final-state) (fare-automata deposit (- current-state final-state)) ] ;; starts change automata and updates stuff
    ; [(> current-state final-state) (fare-automata current-state final-state) ] ;; starts change automata and updates stuff

    [(< current-state final-state) #f ] ;; transaction does not buy a product and db's not updated
  )
)

(define (check-currency-in transactions)
  (cond
    [(null? transactions) #t]
    [(= (car transactions) 1)  (update-money (car transactions) 1) (check-currency-in (cdr transactions)) ]
    [(= (car transactions) 2)  (update-money (car transactions) 1) (check-currency-in (cdr transactions)) ]
    [(= (car transactions) 5)  (update-money (car transactions) 1) (check-currency-in (cdr transactions)) ]
    [(= (car transactions) 10) (update-money (car transactions) 1) (check-currency-in (cdr transactions)) ]
    [(= (car transactions) 20) (update-money (car transactions) 1) (check-currency-in (cdr transactions)) ]
    [(= (car transactions) 50) (update-money (car transactions) 1) (check-currency-in (cdr transactions)) ]
    [else #f]
  )
)

(define (fare-automata deposit debt) ;; in this automata the deposit has to be checked to decide what to return
  ;; debt is input - cost what the machine owes to the user
  ;; debt = (- current-state final-state)

  (cond
    [(= debt 0) #t ] ;; [updates the money and inventory db]
    ;; caar money-deposit is coin value to update in db
    ;; cadar money-deposit is existance

    ;; If debt > 0 then select the first coin that is less or equal to deb
    ;; insert the coin
    [(> debt 0)
     (cond
       [(or (and (< (caar deposit) debt) (> (cadar deposit) 0)) (and (= (caar deposit) debt) (> (cadar deposit) 0))) (return-coin (caar deposit)) (fare-automata (cdr deposit) (- debt (caar deposit))) ]
       [else (fare-automata (cdr deposit) debt)]
     )
    ]
    [else #f] ;; does not update and returns the security-data to the document
  )
)

;; Return the coint to user. I.E erase coin from deposit
(define (return-coin coin)
  (update-money coin -1)
)

;; The down functions will be in helper.rkt
(define (make-copy-inventory)
  "a"
)

(define (make-copy-deposit)
  "b"
)

(define (write-files-db)
  "c"
)

(define (retrieve-copies)
  "d"
)

;; Update inventory according to the transaction result.

;; Any update function should has access to global variables inventory, transactions and deposit to update the info. input data will be data to change inside the update functions. Update writes and reads data from the system.

;; start of the runtime code

transaction
(cln)
inventory
(cln)
deposit
(cln)

;; If transaction happened then return true. Else false. If true, save the data. If not true, retrieve the saved data

(make-copy-inventory)
(make-copy-deposit)

(cond
  [(start-transaction inventory transaction) "::--[Transaction completed]" (write-files-db) ]
  [else "::--[Transaction incompleted]" (retrieve-copies) ]
)

; (cond
;   [(start-transaction inventory transaction) "::--[Transaction completed]"]
;   [else "::--[Transaction incompleted]"]
; )

"After transaction"

inventory
(cln)
deposit
(cln)
