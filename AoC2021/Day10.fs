module AoC2021.Day10

let pointMap =
    Map.ofList [ (')', 3)
                 (']', 57)
                 ('}', 1197)
                 ('>', 25137) ]

type SyntaxError = { expected: char; actual: char option }

type LineResult =
    | Error of SyntaxError
    | Incomplete of char list
    | Failure of string

let parseLine (line: string) =
    let chars = line.ToCharArray() |> Array.toList

    let rec parseLineRec (chars: char list) (brackets: char list) =
        match chars with
        | '(' :: rest -> parseLineRec rest (')' :: brackets)
        | '[' :: rest -> parseLineRec rest (']' :: brackets)
        | '{' :: rest -> parseLineRec rest ('}' :: brackets)
        | '<' :: rest -> parseLineRec rest ('>' :: brackets)
        | ')' :: rest ->
            match brackets with
            | ')' :: t -> parseLineRec rest t
            | err :: t -> Error { expected = ')'; actual = Some err }
            | [] -> Error { expected = ')'; actual = None }
        | ']' :: rest ->
            match brackets with
            | ']' :: t -> parseLineRec rest t
            | err :: t -> Error { expected = ']'; actual = Some err }
            | [] -> Error { expected = ']'; actual = None }
        | '}' :: rest ->
            match brackets with
            | '}' :: t -> parseLineRec rest t
            | err :: t -> Error { expected = '}'; actual = Some err }
            | [] -> Error { expected = '}'; actual = None }
        | '>' :: rest ->
            match brackets with
            | '>' :: t -> parseLineRec rest t
            | err :: t -> Error { expected = '>'; actual = Some err }
            | [] -> Error { expected = '>'; actual = None }
        | [] -> Incomplete brackets
        | _ -> Failure(sprintf "No idea what to do with %s" line)

    parseLineRec chars []

let day10a (input: string list) =
    let grabErrors res =
        match res with
        | Error err -> Some err
        | _ -> None

    let errors =
        List.map parseLine input |> List.choose grabErrors

    let errorToInt (error: SyntaxError) = Map.find error.expected pointMap
    List.map errorToInt errors |> List.sum

let day10b (input: string list) =
    let grabIncompletes res =
        match res with
        | Incomplete bracks -> Some bracks
        | _ -> None

    let pointsB =
        [ (')', 1)
          (']', 2)
          ('}', 3)
          ('>', 4) ]
        |> Map.ofList

    let scoreIncomplete (score: int) (bracket: char) =
        (score * 5) + (Map.find bracket pointsB)

    let lineScores = List.map parseLine input |> List.choose grabIncompletes |> List.map (List.fold scoreIncomplete 0) |> List.sort
    
    let middleIndex = (List.length lineScores / 2)
    List.item middleIndex lineScores
    
    
