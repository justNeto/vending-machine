#lang racket
;;#!/usr/bin/racket -- linux only

;; Provided packages to any other scripts
(provide cln)
(provide update-money)
(provide make-copy-deposit)
(provide make-copy-inventory)
(provide write-files-db)
(provide retrieve-copies)

(provide inventory)
(provide deposit)
(provide transaction)
(provide transactions)
(provide path-to-inventory)
(provide path-to-deposit)

;; Define global variables and parameters
(define inventory null)
(define deposit null)
(define transaction null)
(define transactions null)
(define path-to-inventory null)
(define path-to-deposit null)

;; Define parameters for command-line parser
(define set-inventory (make-parameter #false))
(define set-deposit (make-parameter #false))
(define set-transaction (make-parameter #false))
(define set-transactions (make-parameter #false))

;;
;; helper.rkt defines the initial data for the simulation of the machine and helper functions to be used through the rest of the program
;;

;; Define the start of a new line
(define (cln) (fprintf (current-output-port) "~n"))

;; Creates the [product-list] with entries
;; product-list ::=(([entry])([entry]))that will be used in the automata
;; [entry] ::= ("nombre" price quantity)
(define (create-product-list)
    (define INITIAL_INVENTORY (open-output-file (build-path (current-directory) "db" "product-list")))
	(
 	write '( 	; name price quantity
			("gansito" 18 20)
			("pinguino" 15 20)
			("coca" 20 20)
			("manzanita" 20 20)
			("agua" 15 20)
			("chocoroles" 18 20)
			("fresca" 18 20)
			("fritos" 15 20)
			("ruffles" 15 20)
		)
	INITIAL_INVENTORY
	)
	(close-output-port INITIAL_INVENTORY)
)

;; Creates the [money-deposit] with entries
;; [money-deposit] ::=(([entry])([entry]))that will be used in the automata
;; [entry] ::= (value quantity)
(define (create-money-deposit)
  (define MONEY_INVENTORY (open-output-file (build-path (current-directory) "db" "money-deposit" )))
	(
 	write '(
			(1 20)
			(2 20)
			(5 20)
			(10 20)
			(20 20)
			(50 20)
		)
	MONEY_INVENTORY
	)
	(close-output-port MONEY_INVENTORY)
)

(define (create-transaction-test)
  (define TRANSACTION_INVENTORY (open-output-file (build-path (current-directory) "db" "test-transaction" )))
	(
 	write '(
		    	"gansito"
			(1 1 1 5 10)
		)
	TRANSACTION_INVENTORY
	)
	(close-output-port TRANSACTION_INVENTORY)
)


(define (create-transactions-test)
  (define TRANSACTIONS_INVENTORY (open-output-file (build-path (current-directory) "db" "test-transactions" )))
	(
 	write '(
		    	("gansito" (1 1 1 5 10))
		    	("pinguinos" (1 1 1 5 10))
		    	("fritos" (1 1 1 5 10))
		    	("frescas" (1 1 1 5 10))
		)
	TRANSACTIONS_INVENTORY
	)
	(close-output-port TRANSACTIONS_INVENTORY)
)


(define (update-money index value)
  (set! deposit (reconstruct deposit index value))
)

(define (reconstruct datos index value)
    (cond
     [(null? datos) '()]
     [(eq? index (caar datos)) (append (list (list (caar datos) (+ (cadar datos) value))) (cdr datos))]
     [else (append (list (car datos)) (reconstruct (cdr datos) index value)) ]
    )
)

(define (destroy-product-list)
  (delete-file (build-path (current-directory) "db" "product-list"))
)

(define (destroy-money-deposit)
  (delete-file (build-path (current-directory) "db" "money-deposit"))
)

(define (destroy-transaction-file)
  (delete-file (build-path (current-directory) "db" "test-transaction"))
)

(define (destroy-transactions-file)
  (delete-file (build-path (current-directory) "db" "test-transactions"))
)

;; [Command line parser]
(define parser
  (command-line
    #:program "vending machine"

    #:usage-help "Simulation of a vending machine. If ran without flags it will generate a set of automatic data to test the machine."

    #:once-each
    [("-i" "--inventory") INVENTORY
                     "File with initial product inventory for the machine. Format list: '( (string::product int::price int::quantity) (...) )\n"
                     (set-inventory INVENTORY)]


    [("-d" "--deposit") DEPOSIT
                     "File with initial money deposit for the machine. Format list: '( (int::value int::quantity) (...) )\n"
                     (set-deposit DEPOSIT)]


    #:multi
    [("-t" "--transaction") TRANSACTION
                     "File with a transaction. Format list: '( string::product (int::money int::money int::money ...) ) "
                     (set-transaction TRANSACTION)]


    [("-f" "--file-of-transactions") FILE-TRANSACTIONS
                     "File with a list of transactions. Format list: '( (transaction) (transaction) (...) ) "
                     (set-transactions FILE-TRANSACTIONS)]


    [("-r" "--reset-inventory-deposit")
     			"Deletes current path-to-inventory and path-to-deposit. Use with caution. Do not use if you are using custom files."
                     (delete-inventory-deposit)]


    #:args () (void)
  )
)

;; [get-inventory function]
;; Helper function that handles user input for inventory
(define (get-inventory inv)
  (cond
    [(boolean? inv) (print "::-- [Inventory not selected, using default inventory]")
	(cond
	  [(not (file-exists? (build-path (current-directory) "db" "product-list"))) (create-product-list)
		(cln)
		(print "::-- [List of products created]")
		(cln)
		(define READ_INVENTORY (open-input-file (build-path (current-directory) "db" "product-list")))
		(set! inventory (read READ_INVENTORY))
		(set! path-to-inventory (build-path (current-directory) "db" "product-list"))
		(cln)
	  ]
  	  [(file-exists? (build-path (current-directory) "db" "product-list"))
		(cln)
	  	(print "::-- [The list of available products already exists]")
		(cln)
		(define READ_INVENTORY (open-input-file (build-path (current-directory) "db" "product-list")))
		(set! inventory (read READ_INVENTORY))
		(set! path-to-inventory (build-path (current-directory) "db" "product-list"))
		(cln)
	  ]
	)
    ]

    [(string? inv) (print "::-- [Searching for custom inventory file]")
     (cond
       [(and (file-exists? (build-path (current-directory) inv)) (not(file-exists? (build-path (current-directory) "db" "product-list"))) )
		(cln)
		(print "::-- [File exists. Using it as list of products]")
		(cln)
		(define READ_INVENTORY (open-input-file (build-path (current-directory) inv)))
		(set! inventory (read READ_INVENTORY))
		(set! path-to-inventory (build-path (current-directory) inv))
		(cln)
       ]
       [(and (file-exists? (build-path (current-directory) inv)) (file-exists? (build-path (current-directory) "db" "product-list")) )
		(cln)
		(print "::-- [File exists. Using it as list of products]")
		(cln)
		(destroy-product-list)
		(define READ_INVENTORY (open-input-file (build-path (current-directory) inv)))
		(set! inventory (read READ_INVENTORY))
		(set! path-to-inventory (build-path (current-directory) inv))
		(cln)
       ]
       [else (cln) "::-- [Error. File does not exists]"]
     )
    ]
  )
)

;; [get-deposit function]
;; Helper function that handles user input for deposit
(define (get-deposit dep)
  (cond
    [(boolean? dep) (print "::-- [Money deposit not selected, using default inventory]")
	(cond
	  [(not (file-exists? (build-path (current-directory) "db" "money-deposit")))
		(cln)
	   	(create-money-deposit) (print "::-- [Money deposit created]")
		(cln)
		(define READ_MONEY (open-input-file (build-path (current-directory) "db" "money-deposit")))
		(set! deposit (read READ_MONEY))
		(set! deposit (set-bubble deposit))
		(set! path-to-deposit (build-path (current-directory) "db" "money-deposit"))
		(cln)
	  ]
  	  [(file-exists? (build-path (current-directory) "db" "money-deposit"))
		(cln)
	   	(print "::-- [The money deposit already exists]")
		(cln)
		(define READ_MONEY (open-input-file (build-path (current-directory) "db" "money-deposit")))
		(set! deposit (read READ_MONEY))
		(set! deposit (set-bubble deposit))
		(set! path-to-deposit (build-path (current-directory) "db" "money-deposit"))
		(cln)
	  ]
	)
    ]

    [(string? dep) (print "::-- [Searching for custom deposit file]")
     (cond
       [(and (file-exists? (build-path (current-directory) dep)) (not(file-exists? (build-path (current-directory) "db" "money-deposit"))) )
		(cln)
		(print "::-- [File exists. Using it as money deposit]")
		(cln)
		(define READ_MONEY (open-input-file (build-path (current-directory) dep)))
		(set! deposit (read READ_MONEY))
		(set! deposit (set-bubble deposit))
		(set! path-to-deposit (build-path (current-directory) dep))
		(cln)
       ]
       [(and (file-exists? (build-path (current-directory) dep)) (file-exists? (build-path (current-directory) "db" "money-deposit")) )
		(cln)
		(print "::-- [File exists. Using it as money deposit] ")
		(cln)
		(destroy-money-deposit)
		(define READ_MONEY (open-input-file (build-path (current-directory) dep)))
		(set! deposit (read READ_MONEY))
		(set! deposit (set-bubble deposit))
		(set! path-to-deposit (build-path (current-directory) dep))
		(cln)
       ]
       [else (cln) "::-- [Error. File does not exists]"]
     )
    ]
  )
)

;; [get-transaction function]
;; Helper function that handles passing a single transaction file
(define (get-transaction tran)
  (cond
    [(boolean? tran) (print "::-- [Transaction not selected, using a default transaction]")
	(cond
	  [(not (file-exists? (build-path (current-directory) "db" "test-transaction"))) (create-transaction-test)
		(cln)
		(print "::-- [Transaction created]")
		(cln)
		(define READ_TRANSACTION (open-input-file (build-path (current-directory) "db" "test-transaction")))
		(set! transaction (read READ_TRANSACTION))
		(cln)
	  ]
  	  [(file-exists? (build-path (current-directory) "db" "test-transaction"))
		(cln)
	  	(print "::-- [The transaction file already exists]")
		(cln)
		(define READ_TRANSACTION (open-input-file (build-path (current-directory) "db" "test-transaction")))
		(set! transaction (read READ_TRANSACTION))
		(cln)
	  ]
	)
    ]

    [(string? tran) (print "::-- [Searching for custom transaction file]")
     (cond
       [(and (file-exists? (build-path (current-directory) tran)) (not(file-exists? (build-path (current-directory) "db" "test-transaction"))) ) ; if exist and test use it
		(cln)
		(print "::-- [File exists. Using it as test transaction]")
		(cln)
		(define READ_TRANSACTION (open-input-file (build-path (current-directory) tran)))
		(set! transaction (read READ_TRANSACTION))
		(cln)
       ]
       [(and (file-exists? (build-path (current-directory) tran)) (file-exists? (build-path (current-directory) "db" "test-transaction")) ) ; if exist and test too deletes test
		(cln)
		(print "::-- [File exists. Using it as test transaction]")
		(cln)
		(destroy-transaction-file)
		(define READ_TRANSACTION (open-input-file (build-path (current-directory) tran)))
		(set! transaction (read READ_TRANSACTION))
		(cln)
       ]
       [else (cln) "::-- [Error. File does not exists]"]
     )
    ]
  )
)


;; [get-transaction function]
;; Helper function that handles passing a single transaction file
(define (get-transactions tran)
  (cond
    [(boolean? tran) (print "::-- [Transactions not selected, using a default transactions]")
	(cond
	  [(not (file-exists? (build-path (current-directory) "db" "test-transactions"))) (create-transactions-test)
		(cln)
		(print "::-- [Transactions created]")
		(cln)
		(define READ_TRANSACTIONS (open-input-file (build-path (current-directory) "db" "test-transactions")))
		(set! transactions (read READ_TRANSACTIONS))
		(cln)
	  ]
  	  [(file-exists? (build-path (current-directory) "db" "test-transactions"))
		(cln)
	  	(print "::-- [The transactions file already exists]")
		(cln)
		(define READ_TRANSACTIONS (open-input-file (build-path (current-directory) "db" "test-transactions")))
		(set! transactions (read READ_TRANSACTIONS))
		(cln)
	  ]
	)
    ]

    [(string? tran) (print "::-- [Searching for custom transactions file]")
     (cond
       [(and (file-exists? (build-path (current-directory) tran)) (not(file-exists? (build-path (current-directory) "db" "test-transactions"))) ) ; if exist and test use it
		(cln)
		(print "::-- [File exists. Using it as test transactions]")
		(cln)
		(define READ_TRANSACTIONS (open-input-file (build-path (current-directory) tran)))
		(set! transactions (read READ_TRANSACTIONS))
		(cln)
       ]
       [(and (file-exists? (build-path (current-directory) tran)) (file-exists? (build-path (current-directory) "db" "test-transactions")) ) ; if exist and test too deletes test
		(cln)
		(print "::-- [File exists. Using it as test transactions]")
		(cln)
		(destroy-transactions-file)
		(define READ_TRANSACTIONS (open-input-file (build-path (current-directory) tran)))
		(set! transactions (read READ_TRANSACTIONS))
		(cln)
       ]
       [else (cln) "::-- [Error. File does not exists]"]
     )
    ]
  )
)

(define (read-paths)
  (define PATH_TO_INVENTORY (open-input-file (build-path (current-directory) "db" "tmp" "inv-path")))
  (set! path-to-inventory (string->path (read PATH_TO_INVENTORY)))
  (define PATH_TO_DEPOSIT (open-input-file (build-path (current-directory) "db" "tmp" "dep-path")))
  (set! path-to-deposit (string->path(read PATH_TO_DEPOSIT)))
)

(define (set-inv-path)
  (cond
    [(not (file-exists? (build-path (current-directory) "db" "tmp" "inv-path")))
     (define PATH_INVENTORY (open-output-file (build-path (current-directory) "db" "tmp" "inv-path")))
	(
 	write (path->string path-to-inventory)
	PATH_INVENTORY
	)
	(close-output-port PATH_INVENTORY)
    ]
    [else "Inventory path already set"]
  )
)

(define (set-dep-path)
  (cond
    [(not (file-exists? (build-path (current-directory) "db" "tmp" "dep-path")))
     (define PATH_DEPOSIT (open-output-file (build-path (current-directory) "db" "tmp" "dep-path" )))
	(
 	write (path->string path-to-deposit)
	PATH_DEPOSIT
	)
	(close-output-port PATH_DEPOSIT)
    ]
    [else "Deposit path already set"]
  )
)

;; The down functions will be in helper.rkt
(define (make-copy-inventory)
  (cond
    [(file-exists? (build-path (current-directory) "db" "tmp" "inventory-copy")) (delete-file (build-path (current-directory) "db" "tmp" "inventory-copy")) ]
  )
  (define INVENTORY_COPY (open-output-file (build-path (current-directory) "db" "tmp" "inventory-copy" )))
	(
 	write inventory
	INVENTORY_COPY
	)
	(close-output-port INVENTORY_COPY)
)

(define (make-copy-deposit)
  (cond
    [(file-exists? (build-path (current-directory) "db" "tmp" "deposit-copy")) (delete-file (build-path (current-directory) "db" "tmp" "deposit-copy")) ]
  )
  (define DEPOSIT_COPY (open-output-file (build-path (current-directory) "db" "tmp" "deposit-copy" )))
	(
 	write deposit
	DEPOSIT_COPY
	)
	(close-output-port DEPOSIT_COPY)
)

(define (write-files-db)
  (read-paths)

  (cond
    [(file-exists? path-to-inventory) (delete-file path-to-inventory) ]
  )

  (define UPDATE_INVENTORY (open-output-file path-to-inventory))
	(
 	write inventory
	UPDATE_INVENTORY
	)
	(close-output-port UPDATE_INVENTORY)

  (cond
    [(file-exists? path-to-deposit) (delete-file path-to-deposit) ]
  )

  (define UPDATE_DEPOSIT (open-output-file path-to-deposit))
	(
 	write deposit
	UPDATE_DEPOSIT
	)
	(close-output-port UPDATE_DEPOSIT)
)

(define (delete-inventory-deposit)
  (read-paths)

  (cond
    [(file-exists? path-to-inventory) (delete-file path-to-inventory) ]
  )

  (cond
    [(file-exists? path-to-deposit) (delete-file path-to-deposit) ]
  )
)

(define (retrieve-copies)

  (cond
    [(file-exists? path-to-inventory) (delete-file path-to-inventory) ]
  )

  (cond
    [(file-exists? path-to-deposit) (delete-file path-to-deposit) ]
  )

  (rename-file-or-directory (build-path (current-directory) "db" "tmp" "inventory-copy" ) path-to-inventory)
  (rename-file-or-directory (build-path (current-directory) "db" "tmp" "deposit-copy" ) path-to-deposit)
)

(define (bubble lts)
  (if (null? (cdr lts))
      lts
      (if (> (car (car lts)) (car (cadr lts)))
          (cons (car lts)
                (bubble (cdr lts)))
          (cons (cadr lts)
                (bubble (cons (car lts) (cddr lts))))
      )
  )
)

(define (bubble-sort N lts)
  (cond
        [(= N 1) (bubble lts)]
        [else (bubble-sort (- N 1) (bubble lts))]
  )
)

(define (set-bubble lts)
  (bubble-sort (length lts) lts)
)

;; Execution of setup: creates inventory
"::- [ Set up initial data ] -::"
(cln)
(get-inventory (set-inventory))
(get-deposit (set-deposit))
(get-transaction (set-transaction))
(get-transactions (set-transactions))
(set-dep-path)
(set-inv-path)

"::- [ End setting up data ] -::"
(cln)
