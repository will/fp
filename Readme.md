Based on google's double-conversion and http://www.cs.tufts.edu/~nr/cs257/archive/florian-loitsch/printf.pdf

on mac:
```
➤ crystal run --release test.cr

stdlib: 4.9406564584124654e-324
grisu3: 5.0e-324
stdlib   0.35  (  2.89s ) (± 1.46%)  6.37× slower
grisu3   2.21  (453.31ms) (± 0.61%)       fastest


stdlib: 123.456
grisu3: 123.456
stdlib   1.05  (948.08ms) (± 1.56%)  2.34× slower
grisu3   2.47  (404.55ms) (± 0.42%)       fastest


stdlib: 0.0
grisu3: 0.0
stdlib   3.06  (326.27ms) (± 1.05%)  1.38× slower
grisu3   4.24  (235.87ms) (± 4.74%)       fastest


stdlib: Infinity
grisu3: Infinity
stdlib   3.41  (293.45ms) (± 5.45%)  1.20× slower
grisu3   4.08  (244.86ms) (± 3.19%)       fastest
```

on linux
```
stdlib: 4.9406564584124654e-324
grisu3: 5.0e-324
stdlib    0.7  (  1.43s ) (± 0.10%)  1.96× slower
grisu3   1.37  (729.77ms) (± 0.42%)       fastest


stdlib: 123.456
grisu3: 123.456
stdlib   0.78  (  1.28s ) (± 0.30%)  1.79× slower
grisu3    1.4  (715.03ms) (± 0.50%)       fastest


stdlib: 0.0
grisu3: 0.0
stdlib   1.44  (693.25ms) (± 0.12%)  1.49× slower
grisu3   2.15  (464.87ms) (± 0.91%)       fastest


stdlib: Infinity
grisu3: Infinity
stdlib   2.87  (347.96ms) (± 1.89%)       fastest
grisu3   2.66  (376.19ms) (± 0.82%)  1.08× slower
```
