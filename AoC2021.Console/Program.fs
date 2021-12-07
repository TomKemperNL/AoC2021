// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open AoC2021.Day6

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

[<EntryPoint>]
let main argv =
    day6b 256 [3;4;3;1;2]
    for i in 21 .. 20 .. 10 do
        printfn "%d" i
    
    0 // return an integer exit code