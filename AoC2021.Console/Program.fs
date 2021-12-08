// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open AoC2021.Day6

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

[<EntryPoint>]
let main argv =
    printfn "%f" (1f / 2f)
    
    0 // return an integer exit code