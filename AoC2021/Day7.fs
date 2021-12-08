module AoC2021.Day7

let median (input: int list) : float =
    let sorted = List.sort input
    let l = List.length sorted    
    if l % 2 = 0 then 
        let m1 = (l / 2) - 1
        let i1 = List.item m1 sorted
        let i2 = List.item (m1 + 1) sorted
        float(i1 + i2) / float(2)
    else
        let m = ceil(float(l) / float(2)) |> int
        float(List.item (m - 1) sorted)

let modal (input: int list) =
    List.countBy id input |> List.maxBy snd |> fst
    
let average (input: int list) =
    List.sum input / List.length input

let day7a (input: int list) =    
    let distance a b  = abs(a - b)
    let total target = List.map (distance target) input |> List.sum |> fun n -> (target, n)    
    total (median input |> int)   


let crabDistance a b  =
    let diff = abs(a - b)
    (diff * (1 + diff)) / 2

let day7b (input: int list) =
    let total target = List.map (crabDistance target) input |> List.sum |> fun n -> (target, n)
    let allspots = seq { 0..List.max input } |> Seq.toList
    List.map total allspots |> List.minBy snd
    
    