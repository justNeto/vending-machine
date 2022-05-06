#lang racket
;;#!/usr/bin/racket -- linux only

;; Provided packages to any other scripts
(provide cln)
(provide inventory)
(provide deposit)
(provide transaction)
(provide transactions)

;; Define global variables and parameters
(define inventory null)
(define deposit null)
(define transaction null)
(define transactions null)

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

    #:usage-help "Simulation of a vending machine. If ran"

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
		(cln)
	  ]
  	  [(file-exists? (build-path (current-directory) "db" "product-list"))
		(cln)
	  	(print "::-- [The list of available products already exists]")
		(cln)
		(define READ_INVENTORY (open-input-file (build-path (current-directory) "db" "product-list")))
		(set! inventory (read READ_INVENTORY))
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
		(cln)
       ]
       [(and (file-exists? (build-path (current-directory) inv)) (file-exists? (build-path (current-directory) "db" "product-list")) )
		(cln)
		(print "::-- [File exists. Using it as list of products]")
		(cln)
		(destroy-product-list)
		(define READ_INVENTORY (open-input-file (build-path (current-directory) inv)))
		(set! inventory (read READ_INVENTORY))
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
		(cln)
	  ]
  	  [(file-exists? (build-path (current-directory) "db" "money-deposit"))
		(cln)
	   	(print "::-- [The money deposit already exists]")
		(cln)
		(define READ_MONEY (open-input-file (build-path (current-directory) "db" "money-deposit")))
		(set! deposit (read READ_MONEY))
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
		(cln)
       ]
       [(and (file-exists? (build-path (current-directory) dep)) (file-exists? (build-path (current-directory) "db" "money-deposit")) )
		(cln)
		(print "::-- [File exists. Using it as money deposit] ")
		(cln)
		(destroy-money-deposit)
		(define READ_MONEY (open-input-file (build-path (current-directory) dep)))
		(set! deposit (read READ_MONEY))
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

;; Execution of setup: creates inventory
(get-inventory (set-inventory))
(get-deposit (set-deposit))
(get-transaction (set-transaction))
(get-transactions (set-transactions))
