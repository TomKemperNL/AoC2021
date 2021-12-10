module AoC2021.Day10

let brackets = [
    ('(', ')')
    ('[', ']')
    ('{', '}')
    ('<', '>')
]

let openingBrackets = List.map fst brackets
let closingBrackets = List.map snd brackets
let bracketMap = Map.ofList brackets

type SyntaxError = { expected: char; actual: char option }

type LineResult =
    | Error of SyntaxError
    | Incomplete of char list
    | Failure of string

let parseLine (line: string) =
    let chars = line.ToCharArray() |> Array.toList

    let rec parseLineRec (chars: char list) (brackets: char list) =
        match chars with
        | left :: rest when List.contains left openingBrackets ->
            let matchingBracket = Map.find left bracketMap
            parseLineRec rest (matchingBracket :: brackets)        
        | right :: rest when List.contains right closingBrackets ->
            match brackets with
            | h :: t when h = right -> parseLineRec rest t
            | err :: _ -> Error { expected = right; actual = Some err }
            | [] -> Error { expected = ')'; actual = None }        
        | [] -> Incomplete brackets
        | _ -> Failure(sprintf "No idea what to do with %s" line)

    parseLineRec chars []

let day10a (input: string list) =    
    let pointMap =
        Map.ofList [ (')', 3)
                     (']', 57)
                     ('}', 1197)
                     ('>', 25137) ]
    
    let grabErrors res =
        match res with
        | Error err -> Some err
        | _ -> None

    let errors =
        List.map parseLine input |> List.choose grabErrors

    let errorToInt (error: SyntaxError) = Map.find error.expected pointMap
    List.map errorToInt errors |> List.sum

let day10b (input: string list) : int64 =
    let grabIncompletes res =
        match res with
        | Incomplete bracks -> Some bracks
        | _ -> None

    let pointsB =
        [ (')', 1L)
          (']', 2L)
          ('}', 3L)
          ('>', 4L) ]
        |> Map.ofList

    let scoreIncomplete (score: int64) (bracket: char) =
        (score * 5L) + (Map.find bracket pointsB)

    let lineScores = List.map parseLine input |> List.choose grabIncompletes |> List.map (List.fold scoreIncomplete 0L) |> List.sort
    
    let middleIndex = (List.length lineScores / 2)
    List.item middleIndex lineScores
    
    
