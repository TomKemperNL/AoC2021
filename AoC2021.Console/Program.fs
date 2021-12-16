// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open AoC2021.Day11

// Define a function to construct a message to print
let from whom = sprintf "from %s" whom

let rawInput = """11111
19991
19191
19991
11111"""

[<EntryPoint>]
let main argv =
    let height = 9
    let width = 9
    
    seq {
        for y in (height - 1) .. -1 .. 0 do
            
        for x in (width - 1) .. -1 .. 0 do
            yield (x, y)
    }
    |> Seq.iter (printf "%O")




    0 // return an integer exit code
