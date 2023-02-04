# NUMEX-Interpreter
This project is an Interpreter for a pure functional programming language called NUMEX(Number-Expression Programming Language). NUMEX programs are written using the constructions defined by structs defined at the beginning of project.rkt.
 More information about the NUMEX Interpreter is explained in project_specification.pdf

## Example
### fibonacci function 
```
(define fibb
  (lam "fibb" "n"
       (cnd (orelse (iseq (var "n") (num 0)) (iseq (var "n") (num 1)))
            (num 1)
            (plus (apply (var "fibb") (minus (var "n") (num 1))) (apply (var "fibb") (minus (var "n") (num 2)))))))
```

### Input
```
(eval-exp (apply fibb (num 0)))
```
### Output
```
(num 1)
```

### Input
```
(eval-exp (apply fibb (num 1)))
```
### Output
```
(num 1)
```

### Input
```
(eval-exp (apply fibb (num 2)))
```
### Output
```
(num 2)
```

### Input
```
(eval-exp (apply fibb (num 3)))
```
### Output
```
(num 3)
```

### Input
```
(eval-exp (apply fibb (num 4)))
```
### Output
```
(num 5)
```

### Input
```
(eval-exp (apply fibb (num 5)))
```
### Output
```
(num 8)
```
