module AoC2021.Day10

let pointMap = Map.ofList [
    (')', 3)
    (']', 57)
    ('}', 1197)
    ('>', 25137)
]

type SyntaxError = {
    expected: char
    actual: char option
}



let parseLine (line: string) =
    let chars = line.ToCharArray() |> Array.toList
    
    let rec parseLineRec (chars: char list) (brackets: char list) =
        match chars with
        | '(' :: rest ->
            parseLineRec rest (')'::brackets)
        | '[' :: rest ->
            parseLineRec rest (']'::brackets)
        | '{' :: rest ->
            parseLineRec rest ('}'::brackets)
        | '<' :: rest ->
            parseLineRec rest ('>'::brackets)
        | ')' :: rest ->
            match brackets with
            | ')'::t ->
                parseLineRec rest t
            | err :: t ->
                Some { expected = ')'; actual = Some err }
            | [] ->
                Some { expected = ')'; actual = None }
        | ']' :: rest ->
            match brackets with
            | ']'::t ->
                parseLineRec rest t
             | err :: t ->
                Some { expected = ']'; actual = Some err }
            | [] ->
                Some { expected = ']'; actual = None }
        | '}' :: rest ->
            match brackets with
            | '}'::t ->
                parseLineRec rest t
             | err :: t ->
                Some { expected = '}'; actual = Some err }
            | [] ->
                Some { expected = '}'; actual = None }
        | '>' :: rest ->
            match brackets with
            | '>'::t ->
                parseLineRec rest t
             | err :: t ->
                Some { expected = '>'; actual = Some err }
            | [] ->
                Some { expected = '>'; actual = None }
        | [] ->
            None
        | _ ->
            failwith (sprintf "No idea what to do with %s" line)
    parseLineRec chars []

let day10a (input: string list) =
    let errors = List.choose parseLine input
    let errorToInt (error: SyntaxError) =
        Map.find error.expected pointMap
    List.map errorToInt errors |> List.sum