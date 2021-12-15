namespace AoC2021

module Loops =
    let repeat (n: int) (fn: 'a -> 'a) : 'a -> 'a =
        List.init n (fun _ -> fn) |> List.reduce (>>) 

module Pair =
    let map f (x,y) = (f x, f y)

