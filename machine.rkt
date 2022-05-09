;;#!/usr/bin/racket -- linux only
#lang racket
(require "db/helper.rkt") ;; Creates the data and passes the correct values to this file

;; updates of the db's (inventory and money-deposit) are done by writing files in the system and then reading them.
;; in helper.rkt the logic for update and reading is implemented
;;

(define (start-transactions inventory transactions)
  ;; get product in inventory and confirm it exist
  (cond
    [(null? transactions) #t ]
    [(product-exist? inventory (car transactions)) (start-transactions inventory (cdr transactions)) ] ;; executes the transaction
    [else #f]
  )
)

(define (start-transaction inventory transaction)
  ;; get product in inventory and confirm it exist
  (cond
    [(product-exist? inventory transaction)  #t ] ;; executes the transaction
    [else #f]
  )
)

;; helper function of [start-transaction]
;; if product-name exist then return true
(define (product-exist? inventory transaction)
  (cond
    ;; if inventory null then all inventory checked, if compute-transaction is false then the transaction is not valid, so returns false
    [(null? inventory) #f]
    [(and (and (equal? (caar inventory) (car transaction)) (> (caddar inventory) 0)) (compute-transaction (car inventory) transaction)) (update-inventory (caar inventory) -1) #t ]
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

    ;; validated transactions without fare to return. update inv and money using transactions data
    [(= final-state current-state) #t ]

    ;; Change automata only needs to substract the expected value of the product to the value inputed by the user
    ;; For example, if a product costs 50 and user gave 55, then the machine must return 5 (input - cost)

    [(> current-state final-state) (fare-automata deposit (- current-state final-state)) ] ;; starts change automata and updates stuff
    ; [(> current-state final-state) (fare-automata current-state final-state) ] ;; starts change automata and updates stuff

    [(< current-state final-state) #f ] ;; transaction does not buy a product and db's not updated
  )
)

(define (check-currency-in transactions)
  (cond
    [(null? transactions) #t]
    [(and (< (get-space 1) max-deposit) (= (car transactions) 1)) (update-coin-won 1 1)  (update-money (car transactions) 1) (check-currency-in (cdr transactions)) ] ;; if coin is added to the deposit, also add to a list to register the money won
    [(and (< (get-space 2) max-deposit) (= (car transactions) 2)) (update-coin-won 2 1) (update-money (car transactions) 1) (check-currency-in (cdr transactions)) ]
    [(and (< (get-space 5) max-deposit) (= (car transactions) 5))  (update-coin-won 5 1)(update-money (car transactions) 1) (check-currency-in (cdr transactions)) ]
    [(and (< (get-space 10) max-deposit) (= (car transactions) 10)) (update-coin-won 10 1)(update-money (car transactions) 1) (check-currency-in (cdr transactions)) ]
    [(and (< (get-space 20) max-deposit) (= (car transactions) 20)) (update-coin-won 20 1)(update-money (car transactions) 1) (check-currency-in (cdr transactions)) ]
    [(and (< (get-space 50) max-deposit) (= (car transactions) 50)) (update-coin-won 50 1)(update-money (car transactions) 1) (check-currency-in (cdr transactions)) ]
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
       [(null? deposit) #f ]
       [(or (and (< (caar deposit) debt) (> (cadar deposit) 0)) (and (= (caar deposit) debt) (> (cadar deposit) 0))) (return-coin (caar deposit)) (fare-automata deposit (- debt (caar deposit))) ]
       [else (fare-automata (cdr deposit) debt)]
     )
    ]
    [else #f] ;; does not update and returns the security-data to the document
  )
)

;; Return the coint to user. I.E erase coin from deposit
(define (return-coin coin)
  (update-money coin -1)
  (update-coin-won coin -1)
)

;; Start of runtime code
"::----- { Start runtime data } -----::"
(cln)
" :: --- Before transaction "
(cln)

" :: -  Coin machine won  - :: "
coin-won
(cln)

; " :: - Inventory - :: "
; inventory
; (cln)

; " :: -  Deposit  - :: "
; deposit
; (cln)

" :: -  Transaction  - :: "
transaction
(cln)

" :: -  Transactions  - :: "
transactions
(cln)


;; Conditions to make a transaction
(cond
     [(and (eq? trns-set #t) (eq? trn-set #t)) ;; if using transaction and lists of transactions
      (make-copy-inventory)
      (make-copy-deposit)
      (cond
	[(start-transaction inventory transaction) (print ":: [ Transaction validated ]") ] ;; if transaction ...
	[else (retrieve-copies) (print "::- [ Transaction status: not validated ]") (exit)]
      )
      (cln)
      (cond
	[(start-transactions inventory transactions) (write-files-db) (print ":: [ Transactions status: completed ]") ] ;; ... and transactions are validated then write changes
	[else (retrieve-copies) (print "::- [ Transactions status: not completed ]") (exit)]
      )
      (cln)
     ]
     [(and (eq? trn-set #t)(eq? trns-set #f))
      (make-copy-inventory)
      (make-copy-deposit)
      (cond
	[(start-transaction inventory transaction) (write-files-db) (print ":: [ Transaction status: completed ]") ] ; if only transaction
	[else (retrieve-copies) (print "::- [ Transaction status: not completed ]") (exit)]
      )
      (cln)
     ]
     [else
      (make-copy-inventory)
      (make-copy-deposit)
      (cond
	[(start-transactions inventory transactions) (write-files-db) (print ":: [ Transactions status: completed ]") ] ; if both transactions then
	[else (retrieve-copies) (print "::- [ Transactions status: not completed ]") (exit)]
      )
      (cln)
     ]

)

(cln)
" :: --- After transaction "
(cln)

" :: -  Coin machine won  - :: "
coin-won
(cln)

" :: -  Deposit report  - :: "
(almost-full-coin)
(almost-empty-coin)
(cln)

" :: - Inventory report - :: "
(almost-empty-inventory inventory)
(cln)

; " :: - Inventory - :: "
; inventory
; (cln)

; " :: -  Deposit  - :: "
; deposit
; (cln)

"::----- { Stops runtime data } -----::"
