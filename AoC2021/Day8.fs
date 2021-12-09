module AoC2021.Day8

type Alphabet = A|B|C|D|E|F|G

let translation = Map.ofList [    
    (1, [C;F])
    (7, [A;C;F])
    (4, [B;C;D;F])
    (2, [A;C;D;E;G])
    (3, [A;C;D;F;G])
    (5, [A;B;D;F;G])
    (0, [A;B;C;E;F;G])
    (6, [A;B;D;E;F;G])
    (9, [A;B;C;D;F;G])
    (8, [A;B;C;D;E;F;G])
]
let reverse = Map.toSeq translation |> Seq.map (fun (a,b) -> b,a) |> Map.ofSeq

let parse (line: string) =
    match line.Split("|") with
    | [|patterns;output|] -> 
        let letterToAlphabet (l: char) =
            match l with
            | 'a' -> A
            | 'b' -> B
            | 'c' -> C
            | 'd' -> D
            | 'e' -> E
            | 'f' -> F
            | 'g' -> G
            | _ -> failwith (sprintf "Unknown input %c" l)
        
        let wordToList (word: string) =
            word.Trim().ToCharArray() |> Array.map letterToAlphabet |> Array.toList
            
        let inputs = patterns.Split(" ") |> Array.map wordToList |> Array.toList
        let outputs = output.Split(" ") |> Array.map wordToList |> Array.toList
        (inputs, outputs)
    | _ -> failwith (sprintf "Unable to parse line %s" line)


let translate (word: Alphabet list) : int option =
    match List.length word with
    | 2 -> Some 1
    | 3 -> Some 7
    | 4 -> Some 4
    | 7 -> Some 8
    | _ -> None

let day8a (input: string list) =
    let parsed = List.map parse input
    let outputs = List.map snd parsed
    
    let countnrs targets (words: Alphabet list list) =         
        List.choose translate words |> List.filter (fun i -> List.contains i targets) |> List.length
    
    List.map (countnrs [1;4;7;8]) outputs |> List.sum 
    
let day8b input =
    42