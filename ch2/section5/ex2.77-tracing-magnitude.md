# Tracing call to `magnitude` procedure

```lisp
>(magnitude '(complex rectangular 3 . 4))
>(apply-generic 'magnitude '(complex rectangular 3 . 4))
>(magnitude '(rectangular 3 . 4))
>(apply-generic 'magnitude '(rectangular 3 . 4))
>(magnitude '(3 . 4))
<5
5
```
