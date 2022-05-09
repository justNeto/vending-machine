# vending-machine

## Linux
To run the vending machine program use
```racket
racket machine.rkt -s
```

This will run the default auto-generated data for a simulation. You can use custom data at any time (such as files for a transaction or a list of transactions) by using the proper flags. I strongly suggest to run the below command to see all the proper format for each file.
```racket
racket machine.rkt -h
```

A sample file for each flag is shown below.

transaction-test-file
```racket
("coca"(1 1 1 5 5 5))
```

transactions-test-file
```racket
(("coca" (1 1 1 5 10))("chocoroles" (1 1 1 5 10))("agua" (1 1 1 5 10)))
```

money-deposit
```racket
((5 50)(2 10)(1 20))
```

product-inventory
```racket
(("coca" 15 10)("agua" 15 12)("manzanita" 7 10))
```

These sample files, besides the default generated, can be found in tests directory of this repository.
