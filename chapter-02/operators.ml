let multiplier x y = x * y

let divider x y = x /. y

let rec power (x:float) (y:int) =
  if y = 0 then 1.0
  else x *. power x (y - 1)