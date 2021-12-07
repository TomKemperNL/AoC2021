module AoC2021.Day6

open System
open System.Text.RegularExpressions
open System.Text

let debugPrint day (fishies: int list) = 
    let sb = StringBuilder()

    for f in fishies do
        sb.Append(String.Format("{0},",f)) |> ignore

    printfn "%3d - %3d %s" day (List.length fishies) (sb.ToString())

let rec fishies targetday (fishTodo: int list) (fishDone: int list) day newFishies =
    if fishDone = [] then 
        debugPrint (day) fishTodo
    if day = targetday then
        fishTodo
    else
        match fishTodo with 
        | (0 :: t) -> 
            fishies targetday t (6 :: fishDone) day (8 :: newFishies)
        | (h :: t) -> 
            fishies targetday t (h - 1 :: fishDone) day newFishies
        | [] -> 
            let newGeneration = List.append (List.rev fishDone) newFishies             
            fishies targetday newGeneration [] (day + 1) []



let rec singleFishie reset newFishie days fishie = 
    if days < fishie then
        1
    else
        let effectiveDays = days - fishie - 1
        let period = reset + 1 // (include zero for a cycle)        
        let descendants = 
            seq {                 
                for r in 0 .. period .. effectiveDays do 
                    yield singleFishie reset newFishie (effectiveDays - r) newFishie      
            } |> Seq.sum
        1 + descendants

let day6a days (input: int list) =
    //let endState = fishies days input [] 0 []
    //List.length endState
    let fishie = singleFishie 6 8 days
    List.map fishie input |> List.sum


let day6b days (input: int list) =
    let cache = new System.Collections.Generic.Dictionary<_,_>()
    printfn "6ben!"
    let fishie = singleFishie 6 8 days
    let memoFishie d = 
        match cache.TryGetValue d with
        | true, v -> 
            printfn "cache hit for %d" v
            v
        | false, _ ->
            let v = fishie d
            printfn "cache miss for %d"
            cache.Add(d,v)            
            v

    List.map memoFishie input |> List.sum