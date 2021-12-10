// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open AoC2021.Day8

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

[<EntryPoint>]
let main argv =
    
    let line = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
    let (inp, out) = parse line
    
    match tryTranslate inp with
    | Some map -> printMap map
        
    | None -> printfn "Noooope"
    
    
    0 // return an integer exit code