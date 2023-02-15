# interval-evaluator

A custom programming language Interevaluator for evaluating intervals written with [Racket](https://racket-lang.org/). Programming assignment for Functional Programming course at the University of Ljubljana, Faculty of Computer and Information Science in 2019. Its purpose was to put into practice what we learned about programming language theory.

File navodila.pdf contains detailed instructions in the Slovene language. 

## Syntax
To briefly summarize our programming language:

* **(iv exp env)**: Interevaluator expression **exp** evaluated in its local environment **env**

* Data types
  * **(const a)**: number value
  * **(bool b)**: boolean value
  * **(interval a b)**: interval [**a**, **b**]
  * **(pair e1 e2)**: pair of values of expressions **e1** and **e2**
  * **(nil)**: null value
  
* Operations
  * **(if-then-else b e1 e2)**: branching
  * **(is-const? e)**, **(is-bool? e)**, **(is-interval? e)**, **(is-nil? e)**: data type enquiry
  * **(negate e)**: negation
  * **(add e1 e2)**: sumation
  * **(multiply e1 e2)**: multiplication
  * **(exponentiate e)**: exponential function
  * **(left e)**, **(right e)**: extraction - returns left or right value of pair or interval
  * **(greater e1 e2)**: returns true if **e1** > **e2** and false if **e1** <= **e2**
  * **(intersect e1 e2)**: returns interval containing intersection of intervals resulting from expresions **e1** and **e2**
  
* Variables
  * **(with vars e)**: expand the current local environment with a hash table of variable names and their expressions **vars**, and evaluate expression **e** in this expanded environment
  * **(valof s)**: read local value of variable defined with a string **s**

* Functions
  * **(function name farg body)**: define a function
  * **(script name body)**: define a script
  * **(call e arg)**: call a function or a script **e**. If **e** is a script arg must be (nil)
  
* Macros:
  * **(subtract e1 e2)**: generates Interevaluator's expression (add **e1** (negate **e2**)))
  * **(lower e1 e2)**: generates Interevaluator's expression that checks if **e1** is smaller than **e2**
  * **(equal e1 e2)**: generates Interevaluator's expression that checks if **e1** is equal to **e2**
  * **(encloses i1 i2)**: generates Interevaluator's expression that checks if interval **i1** is enclosed in interval **i2**


## Example

define a factorial function and call it to calculate a factorial of number 5:

(iv (call
        (function "fact" (list "n")
            (if-then-else (greater (const 1) (valof "n"))
                (const 1)
                (multiply (valof "n") (call (valof "fact") (list (subtract (valof "n") (const 1)))))))
        (list (const 5)))
    (hash))
