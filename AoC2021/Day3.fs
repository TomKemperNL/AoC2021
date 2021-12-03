module AoC2021.Day3

open System.Collections.Specialized

[<Struct>]
type Counts = {
    ones: int
    zeroes: int
    other: int
}

let toIntArray (line: string) =
    let bv = Array.create line.Length 0
    let chars: char[] = line.ToCharArray()
    for ((ix: int), (c: char)) in Array.indexed chars do
        bv.[ix] <- if c = '1' then 1 else 0   
    bv

let toDecimal (bits: int[]) = 
    let reversed = Array.rev bits
    let ixrev = Array.indexed reversed
    let step (total: int) ((ix: int), (i: int)) = 
        total + (i * (pown 2 ix))
    Array.fold step 0 ixrev

let day3a (input: string list) =
    match input with
    | a :: _ ->
        let width = a.Length
        let start = Array.create width { ones= 0; zeroes= 0; other= 0 }
        
        let step (state: Counts[]) (item: string) : Counts[] =
            let chars : char[] = item.ToCharArray()
            let update (count: Counts) (ch: char) = 
                match ch with
                | '1' -> { count with ones = count.ones + 1 }
                | '0' -> { count with zeroes = count.zeroes + 1 }
                | _ -> { count with other = count.zeroes + 1 }
            Array.map2 update state chars

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

            
        let result = List.fold step start input
        let (gamma, epsilon) = gammaEpsilonSplit result
        let g2 = toDecimal gamma
        let e2 = toDecimal epsilon
        g2 * e2
        
    | _ -> 0
    
    