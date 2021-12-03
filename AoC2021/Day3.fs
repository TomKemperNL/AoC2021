﻿module AoC2021.Day3

open System.Collections.Specialized

[<Struct>]
type Counts = {
    ones: int
    zeroes: int
    other: int
}

let toDecimal (bits: int[]) = 
    let reversed = Array.rev bits
    let ixrev = Array.indexed reversed
    let step (total: int) ((ix: int), (i: int)) = 
        total + (i * (pown 2 ix))
    Array.fold step 0 ixrev

let toIntArray (line: string) = 
    Array.map (fun c -> if c = '1' then 1 else 0) (line.ToCharArray())

let countCommons (input: string list) = 
    let width = (List.head input).Length
    let start = Array.create width { ones= 0; zeroes= 0; other= 0 }
    
    let step (state: Counts[]) (item: string) : Counts[] =
        let chars : char[] = item.ToCharArray()
        let update (count: Counts) (ch: char) = 
            match ch with
            | '1' -> { count with ones = count.ones + 1 }
            | '0' -> { count with zeroes = count.zeroes + 1 }
            | _ -> { count with other = count.zeroes + 1 }
        Array.map2 update state chars

                 
    List.fold step start input

let day3a (input: string list) =
    match input with
    | a :: _ ->
        let gammaEpsilonSplit (counts: Counts[]) : (int[] * int[]) = 
               let split (c: Counts) = 
                   match c with
                   | c when c.ones > c.zeroes -> (1, 0)
                   | c when c.zeroes > c.ones -> (0, 1)
                   | _ -> failwith "Ties are undefined"
               let items = Array.map split counts
           
               let mutable gamma = Array.create counts.Length 0
               let mutable epsilon = Array.create counts.Length 0
               for ix, (g, e) in Array.indexed items do
                   gamma.[ix] <- g
                   epsilon.[ix] <- e
               (gamma, epsilon)  

        let result = countCommons input
        let (gamma, epsilon) = gammaEpsilonSplit result
        let g2 = toDecimal gamma
        let e2 = toDecimal epsilon
        g2 * e2        
    | _ -> 0
    
let day3b (input: string list) = 
    let results = countCommons input

    let rec findOxRating (inputs: string list) pos =
        let counts = countCommons inputs
        match inputs with 
        | [a] -> a
        | _ -> 
            let applicableCounts = counts.[pos]
            let target = if applicableCounts.ones >= applicableCounts.zeroes then '1' else '0' 
            let isApplicable (line: string) = 
                line.ToCharArray().[pos] = target
            let remaining = List.filter isApplicable inputs
            findOxRating remaining (pos + 1)

    let rec findCo2Rating (inputs: string list)  pos =
        let counts = countCommons inputs
        match inputs with 
        | [a] -> a
        | _ -> 
            let applicableCounts = counts.[pos]
            let target = if applicableCounts.ones >= applicableCounts.zeroes then '0' else '1' 
            let isApplicable (line: string) = 
                line.ToCharArray().[pos] = target
            let remaining = List.filter isApplicable inputs
            findCo2Rating remaining (pos + 1)

   
    let ox = findOxRating input 0 |> toIntArray |> toDecimal
    let co2 = findCo2Rating input 0 |> toIntArray |> toDecimal

    ox * co2


            
