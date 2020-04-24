let double x = x * 2

let cube x = x *. x *. x

let sign x =
  if x <> 0 then
    (if x > 0 then 1 else -1)
  else 0

let circle_area r =
  let pi = 2.0 *. asin 1.0 in pi *. r *. r

let rms x y = sqrt(((x*.x)+.(y*.y))/.2.)