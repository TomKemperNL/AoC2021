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
    let s : string = string([|'a';'b'|]) 
    printfn "bla %s" s



    0 // return an integer exit code
