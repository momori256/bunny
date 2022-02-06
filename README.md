# bunny interpreter

An interpreter implemented with OCaml.

```
$ dune exec bin/bunny.exe
Welcome to bunny REPL.
To exit, type "exit" or "quit".
> 
```

## calculation
```
> (3 - (1 + 4)) * -15
30

> (1 + 4)!
120

> 3 <> 5
true
> 4 = 1 + 3
true
> ~true
false
```

- prefix `+`/`-`/`~`
- infix `+`/`-`/`*`/`=`/`<>`
- suffix `!`
- grouping `()`

## if expression
```
> if (3 < 5) { if (~true) { 1 } else { 2 } } else { 3 }
2
```

## variable
```
> let x = 1 in let y = 2 in let z = 3 in x + y + z
6

> let x = 10
(x -> 10)
> let y = 20
(y -> 20)
> x + y
30
```

## function
```
> let twice = fun (f, x) { f(f(x)) }    
(twice -> (fun (f, x) { (f ((f (x)))) }))
> twice(fun (x) { x * x }, 5)
625

> let fact = fun (x) { if (x = 0) { 1 } else { x * fact(x-1) } }
(fact -> (fun (x) { (if ((x = 0)) then (1) else ((x * (f ((x - 1)))))) }))
> fact (5)
120

```

# Test
```
$ dune exec -- test/main.exe
(usage) dune exec -- test/main.exe ([lexer|parser|eval|example]+)

$ dune exec -- test/main.exe lexer
<lexer test> OK      
	(single) OK
	(double) OK
	(special) OK
	(int) OK
	(identifier) OK
```


# Reference
[Go言語でつくるインタプリタ](https://www.oreilly.co.jp/books/9784873118222/)
