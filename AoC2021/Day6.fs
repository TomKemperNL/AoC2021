module AoC2021.Day6

open System
open System.Text.RegularExpressions


let rec fishies targetday (fishTodo: int list) (fishDone: int list) day newFishies =
    if day = targetday then
        fishTodo
    else
        match fishTodo with 
        | (0 :: t) -> 
            fishies targetday t (6 :: fishDone) day (8 :: newFishies)
        | (h :: t) -> 
            fishies targetday t (h - 1 :: fishDone) day newFishies
        | [] -> 
            let newGeneration = List.append fishDone newFishies 
            fishies targetday newGeneration [] (day + 1) []

let day6a days (input: int list) =
    let endState = fishies days input [] 0 []
    List.length endState