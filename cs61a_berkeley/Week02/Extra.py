# Recursion with just lambda.

print(
    (lambda a, b : (
        (lambda square : (
            square(a) + square(b)
        ))(lambda x : x * x)
    ))(3, 4)
)

print(
    (lambda n : (
        (lambda fact : (
            fact(fact, n)
        ))(lambda fact, n : ((
                1
            ) if n == 0 else (
                n * fact(fact, n - 1)
            )
        ))
    ))(5)
)
