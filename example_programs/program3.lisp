(
    (fn add (x y) (+ x y))
    (let x 4)
    (let z 2)
    (if (= (add x z) 6)
        (print Success)
        (print Failure)
    )
)