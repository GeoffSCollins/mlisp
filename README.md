# mlisp
 
# mLisp. A minimal version of the programming language Lisp written in Rust.
 
Core functionalities of mLisp
- Define variables using the following syntax(let var <Expr>)
- Define functions using the following syntax(fn function (arg1 arg2 arg3) <Expr>)where the  Expr is the function body. The second element of the function definition is a list of parameter names. 
- If the variable stores a number (within the scope of the variable), the expression will be returned. Otherwise, the symbol itself is returned.
- If statements use the following syntax (if (<Expr>) (<Expr>) (<Expr>))where the first Expr is the predicate, the second Expr is the expression that is run if the predicate is true, the final Expr is the expression that is run if the predicate is false.
- Logical operations and, or, and not can be used. For example, (and True True False) will compute False as all arguments must be True. 
- Logical comparison operators = and != can be used. For example, (= True False True) will return False.
- Basic math operations (+, -, *, /) can be used. For example, (+ 10 4 6 3) is equivalent to 10 + 4 + 6 + 3 = 23. Multiplication and division follow the same pattern. For example, (/ 40 10 4) will compute 1. 
- Values, symbols, and functions can be printed to stdout using the print function. For example, (print func) will print “<func-object: func>” and (print x) will print “x”.


Example programs:

cat program1.lisp
```lisp
(
    (let x (+ 3(- 5 3 1)))
    (print x)
    (let zeta 100)
    (let early (/ zeta 10))
    (let tmp (/ x early))
    (let y tmp)
    (print y)
    (if (= 0.4 y) 
        (print Correct_Answer)
        (print Incorrect)
    )
)
```

cargo run progam1.txt
```
4 0.4 Correct_Answer
```
