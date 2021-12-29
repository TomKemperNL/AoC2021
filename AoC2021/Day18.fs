module AoC2021.Day18

open System

type Value =
    | Raw of int
    | Pair of Value * Value

type SnailNumber = Value * Value




let fromString (input: string) =
    let isNumeric c = Seq.contains c (seq { '0' .. '9' })
    let toNr chars =
        let nrs = List.rev chars |> List.toArray |> fun ns -> new System.String(ns)
        int nrs |> Raw

    let rec readValue (todo: char list) (readnrs: char list) (values: Value list) =
        match todo with
        | [] ->
            match values with
            | [result] -> result
            | _ -> failwith "Uuurgh"
        | '[' :: rest ->
            readValue rest readnrs values //Dit klopt niet...
        | ']' :: rest ->
            let values = match readnrs with
                            | [] -> values
                            | nrs -> toNr nrs :: values
            match values with
            | a :: (b :: remaining) ->
                readValue rest [] (Pair (b,a) :: remaining)
            | _ -> failwith "uuuuuurgh2"
        | ',' :: rest ->
            let values = match readnrs with
                            | [] -> values
                            | nrs -> toNr nrs :: values
            readValue rest [] values
        | c :: rest when isNumeric c -> 
            readValue rest (c :: readnrs) values
        | _ -> failwith "wut"



    let result =
        readValue (input.ToCharArray() |> List.ofArray) [] []

    match result with

    | Raw x -> failwith "Blergh"
    | Pair (left, right) -> (left, right)

let toString ((left, right): SnailNumber) =
    let rec valueToString (v: Value) : string =
        match v with
        | Raw x -> x.ToString()
        | Pair (left, right) -> sprintf "[%s,%s]" (valueToString left) (valueToString right)

    sprintf "[%s,%s]" (valueToString left) (valueToString right)


let add (left: SnailNumber) (right: SnailNumber) : SnailNumber = (Pair left, Pair right)

let tryExplode (n: Value) =
    let rec explodeRec nr steps =
        match nr, steps with
        | Raw n, _ -> false, nr
        | Pair (left, right), 4 -> false, nr

    explodeRec n 1

let reduce ((left, right): SnailNumber) = 42
