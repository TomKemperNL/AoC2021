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
    
    List.map (distance 1) input |> List.sum |> (printfn "distance to 1 %d")
    List.map (distance 3) input |> List.sum |> (printfn "distance to 3 %d")
    List.map (distance 10) input |> List.sum |> (printfn "distance to 10 %d")
    
    let total = List.map (distance middle) input |> List.sum
    (middle, total)