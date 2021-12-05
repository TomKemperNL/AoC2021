// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open AoC2021.Day5

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

[<EntryPoint>]
let main argv =
    printfn "%A" (points ((1,0), (4,3)))
    printfn "%A" (points ((3,3), (0,0)))
    printfn "%A" (points ((0,3), (3,0)))
    printfn "%A" (points ((3,0), (0,3)))
    0 // return an integer exit code