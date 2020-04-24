let rec fib position =
  if position = 1 || position = 2 then 1
  else (fib(position-1) + fib(position-2))

let rec fib_helper pp p n =
  if n = 1 then p
  else fib_helper p (pp+p) (n-1)
and fib_fast n = fib_helper 0 1 n

let (+/.) x y = (x +. y) /. 2.
