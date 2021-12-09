module AoC2021.Day8

type Alphabet =
    | A
    | B
    | C
    | D
    | E
    | F
    | G

type Wiring = Map<Alphabet, Alphabet>
type WiringOptions = Map<Alphabet, Alphabet list>

let fromInt =
    Map.ofList [ (1, [ C; F ])
                 (7, [ A; C; F ])
                 (4, [ B; C; D; F ])
                 (2, [ A; C; D; E; G ])
                 (3, [ A; C; D; F; G ])
                 (5, [ A; B; D; F; G ])
                 (0, [ A; B; C; E; F; G ])
                 (6, [ A; B; D; E; F; G ])
                 (9, [ A; B; C; D; F; G ])
                 (8, [ A; B; C; D; E; F; G ]) ]

let toInt =
    Map.toSeq fromInt
    |> Seq.map (fun (a, b) -> b, a)
    |> Map.ofSeq

let parse (line: string) =
    match line.Split("|") with
    | [| patterns; output |] ->
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
            word.Trim().ToCharArray()
            |> Array.map letterToAlphabet
            |> Array.toList

        let inputs =
            patterns.Split(" ")
            |> Array.filter (fun w -> w.Trim().Length > 0)
            |> Array.map wordToList
            |> Array.toList

        let outputs =
            output.Split(" ")
            |> Array.filter (fun w -> w.Trim().Length > 0)
            |> Array.map wordToList
            |> Array.toList

        (inputs, outputs)
    | _ -> failwith (sprintf "Unable to parse line %s" line)

let sup =
    let all = [ A; B; C; D; E; F; G ]
    Map.ofList (List.map (fun x -> (x, all)) all)

let intersect current options =
    List.filter (fun c -> List.contains c options) current

let intersectMap map key options =
    let existing = Map.find key map
    let replacement = intersect existing options
    Map.add key replacement map

let processClue (wiring: WiringOptions) (word: Alphabet list) =
    let intersect key options map = intersectMap map key options

    match word with
    | [ c; f ] ->
        wiring
        |> intersect c [ C; F ]
        |> intersect f [ C; F ]
    | [ a; c; f ] ->
        wiring
        |> intersect a [ A; C; F ]
        |> intersect c [ A; C; F ]
        |> intersect f [ A; C; F ]
    | [ b; c; d; f ] ->
        wiring
        |> intersect b [ B; C; D; F ]
        |> intersect c [ B; C; D; F ]
        |> intersect d [ B; C; D; F ]
        |> intersect f [ B; C; D; F ]
    | [ a; b; c; d; e ] ->
        wiring //eeeeuuuuh
    | [ a; b; c; d; e; f ] ->
        wiring //eeeeuuuuh
    | [ a; b; c; d; e; f; g ] -> wiring //no info        
    | _ ->
        failwith "lolwut"

let printMap (translation: WiringOptions) =
    for (k, v) in Map.toList translation do
        printfn "%O: %A" k v

let rec fixedPoint fn items =
    let once = fn items

    if (fn once) = once then
        once
    else
        fixedPoint fn once

let tryTranslate (words: Alphabet list list) =
    let startState = sup
    let run translationInProgress =
        List.fold processClue translationInProgress words
    fixedPoint run startState

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
        List.choose translate words
        |> List.filter (fun i -> List.contains i targets)
        |> List.length

    List.map (countnrs [ 1; 4; 7; 8 ]) outputs
    |> List.sum

let day8b input = 42
