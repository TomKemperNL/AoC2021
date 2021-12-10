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
type WiringOptions = Map<Alphabet, Set<Alphabet>>

let fromInt =
    Map.ofList [ (1, [ C; F ] |> Set.ofList)
                 (7, [ A; C; F ] |> Set.ofList)
                 (4, [ B; C; D; F ] |> Set.ofList)
                 (2, [ A; C; D; E; G ] |> Set.ofList)
                 (3, [ A; C; D; F; G ] |> Set.ofList)
                 (5, [ A; B; D; F; G ] |> Set.ofList)
                 (0, [ A; B; C; E; F; G ] |> Set.ofList)
                 (6, [ A; B; D; E; F; G ] |> Set.ofList)
                 (9, [ A; B; C; D; F; G ] |> Set.ofList)
                 (8, [ A; B; C; D; E; F; G ] |> Set.ofList) ]

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

let all = [ A; B; C; D; E; F; G ] |> Set.ofList

let sup =    
    Map.ofSeq (Set.map (fun x -> (x, all)) all)

let intersectMap map key options =
    let existing = Map.find key map
    let replacement = Set.intersect existing options
    Map.add key replacement map
    
let diffMap map key toRemove =
    let existing = Map.find key map
    let replacement = Set.difference existing toRemove
    Map.add key replacement map

let processClue (wiring: WiringOptions) (word: Alphabet list) =
    let intersect key options map = intersectMap map key options

    match word with
    | [ c; f ]  ->
        let matches = [ C; F ] |> Set.ofList
        
        wiring
        |> intersect c matches
        |> intersect f matches
    | [ a; c; f ] ->
        let matches = [ A; C; F ] |> Set.ofList
        
        wiring
        |> intersect a matches
        |> intersect c matches
        |> intersect f matches
    | [ b; c; d; f ] ->
        let matches = [ B; C; D; F ] |> Set.ofList
        
        wiring
        |> intersect b matches
        |> intersect c matches
        |> intersect d matches
        |> intersect f matches
    | w when List.length w = 5 ->
        let missingFrom = [B;C;E;F;] |> Set.ofList
        //eeeuh dit kan beter hier gooi ik informatie weg. Nouja, misschien hebben we mazzel
        let filterWiring (w: WiringOptions) (a: Alphabet) =
            intersectMap w a missingFrom
        
        let missingLetters = Set.difference all (Set.ofList word)
        Set.fold filterWiring wiring missingLetters       
    | w when List.length word = 6 ->
        let missingFrom = [ C;D;E ] |> Set.ofList        
        let filterWiring (w: WiringOptions) (a: Alphabet) =
            intersectMap w a missingFrom
        
        let missingLetters = Set.difference all (Set.ofList word)
        Set.fold filterWiring wiring missingLetters       
    | [ a; b; c; d; e; f; g ] -> wiring //no info        
    | _ ->
        failwith "lolwut"

let resolveCandidates (wiring: WiringOptions) =
    let tupled = Map.toList wiring
    
    let updateWiring (w: WiringOptions) (key, value) : WiringOptions =
        let allKeys = Map.keys w |> Seq.toList
        let occurences = List.filter (snd >> (=) value) tupled
        let keys = List.map fst occurences
        if List.length occurences = Set.count value then
            List.fold (fun state k -> diffMap state k value) w (List.except keys allKeys)       
        else
            w
    
    List.fold updateWiring wiring tupled
    
        
    
let printMap translation =
    for (k, v) in Map.toList translation do
        printfn $"{k}: {v}"

let rec fixedPoint fn items =
    let once = fn items

    if (fn once) = once then
        once
    else
        fixedPoint fn once



let tryTranslate (words: Alphabet list list) =
    let startState = sup
    let run translationInProgress =
        let processResult = List.fold processClue translationInProgress words
        resolveCandidates processResult
    let result = fixedPoint run startState
    if Map.values result |> Seq.forall (Set.count >> (=) 1) then
        Some (Map.map (fun k v -> (Set.toList >> List.item 0) v) result)
    else
        None
        
let wiringToNumber (translation: Map<Alphabet,Alphabet>) (wires: Alphabet list) : int =
    let activeWires = List.map (fun n -> Map.find n translation) wires |> Set.ofList
    Map.find activeWires toInt

let concatNrs (nrs: int list) : int=
    let conc = String.concat ""
    List.map string nrs |> conc |> int

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

let day8bSingle line = 
    let (inp, out) = parse line

    match tryTranslate inp with
    | Some map ->
        printMap map |> ignore
        List.map (wiringToNumber map) out |> concatNrs
    | None -> failwith (sprintf "Cannot translate %s" line)

let day8b (input: string list) =
    List.map day8bSingle input |> List.sum