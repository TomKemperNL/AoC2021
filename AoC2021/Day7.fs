module AoC2021.Day7

let median (input: int list) =
    let sorted = List.sort input
    let l = List.length sorted
    
    if l % 2 = 0 then 
        let m1 = ceil(float(l) / float(2)) |> int
        let m2 = floor(float(l) / float(2)) |> int
        ((List.item m1 input) + (List.item m2 input)) / 2
    else
        let m = ceil(float(l) / float(2)) |> int
        List.item m input
        

let day7a (input: int list) =
    let middle = median input 
    let distance a b  = abs(a - b)    
    
    let total target = List.map (distance target) input |> List.sum |> fun n -> (target, n)
    
    List.map total input |> List.minBy snd
    
    