// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open AoC2021.Day8

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

[<EntryPoint>]
let main argv =
    printfn "%A" ("16,1,2,0,4,2,7,1,2,14".Split(",")|> Array.map int |> Array.sort)
    
    0 // return an integer exit code