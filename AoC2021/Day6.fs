module AoC2021.Day6

open System
open System.Text.RegularExpressions
open System.Text

let debugPrint day (fishies: int list) = 
    let sb = StringBuilder()

    for f in fishies do
        sb.Append(String.Format("{0},",f)) |> ignore

    printfn "%3d - %3d %s" day (List.length fishies) (sb.ToString())

let rec slowFishies targetday (fishTodo: int list) (fishDone: int list) day newFishies =
    if fishDone = [] then 
        debugPrint (day) fishTodo
    if day = targetday then
        fishTodo
    else
        match fishTodo with 
        | (0 :: t) -> 
            slowFishies targetday t (6 :: fishDone) day (8 :: newFishies)
        | (h :: t) -> 
            slowFishies targetday t (h - 1 :: fishDone) day newFishies
        | [] -> 
            let newGeneration = List.append (List.rev fishDone) newFishies             
            slowFishies targetday newGeneration [] (day + 1) []



let singleFishie (cache: System.Collections.Generic.Dictionary<int,bigint>) reset newFishie days fishie =
    let period = reset + 1 // include zero in period
    let effectiveDays = days - fishie - 1 // start at 0
    let rec normalizedFishie days =        
        match cache.TryGetValue(days) with
        | true, v -> 
            v
        | false, _ ->
            let descendants = 
                seq {                 
                    for r in 0 .. period .. days do 
                        yield normalizedFishie (days - r - newFishie - 1)
                } |> Seq.sum
            let result = bigint(1) + descendants
            cache.Add(days, result)
            result

    normalizedFishie effectiveDays

let day6a days (input: int list) =
    //let endState = fishies days input [] 0 []
    //List.length endState

    let cache = new System.Collections.Generic.Dictionary<int,bigint>()
    let fishie = singleFishie cache 6 8 days
    List.map fishie input |> List.sum


let day6b days (input: int list) =
    let cache = new System.Collections.Generic.Dictionary<int,bigint>()
    let fishie = singleFishie cache 6 8 days
    List.map fishie input |> List.sum