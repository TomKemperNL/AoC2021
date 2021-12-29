module AoC2021.Day18

open System

type Value =
    | Raw of int
    | Pair of Value * Value

type SnailNumber = Value * Value




let fromString (input: string) =
    let isNumeric c = Seq.contains c (seq { '0' .. '9' })

    let toNr chars =
        let nrs =
            List.rev chars
            |> List.toArray
            |> fun ns -> new System.String(ns)

        int nrs |> Raw

    let rec readValue (todo: char list) (readnrs: char list) (values: Value list) =
        match todo with
        | [] ->
            match values with
            | [ result ] -> result
            | _ -> failwith "Uuurgh"
        | '[' :: rest -> readValue rest readnrs values //Dit klopt niet...
        | ']' :: rest ->
            let values =
                match readnrs with
                | [] -> values
                | nrs -> toNr nrs :: values

            match values with
            | a :: (b :: remaining) -> readValue rest [] (Pair(b, a) :: remaining)
            | _ -> failwith "uuuuuurgh2"
        | ',' :: rest ->
            let values =
                match readnrs with
                | [] -> values
                | nrs -> toNr nrs :: values

            readValue rest [] values
        | c :: rest when isNumeric c -> readValue rest (c :: readnrs) values
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

type ExplodeProgress = (int option * int option) option

let rec addLeft (value: int) (n: Value) : bool*Value =
    match n with
    | Raw x -> true, Raw (x + value)
    | Pair (l,r) ->
        let success, lresult = addLeft value l
        success, Pair (lresult, r)


let rec addRight (value: int) (n: Value) : bool*Value =
    match n with
    | Raw x -> true, Raw (x + value)
    | Pair (l,r) ->
        let success, rresult = addRight value r
        success, Pair (l, rresult)

let tryExplode (n: Value) : (ExplodeProgress * Value)=
    let rec explodeRec nr steps : ExplodeProgress * Value=
        match nr, steps with     
        | Raw n, _ ->
            None, Raw n
        | Pair (left, right), 4 ->
            match left, right with
            | Raw n, Raw m -> 
                Some (Some n, Some m), Raw 0
            | _ ->
                failwith "Exploding pairs will always consist of two regular numbers?"
        | Pair (left, right), s -> 
            let tryLeft = explodeRec left (s+1)
            match tryLeft with
            | Some targets, leftResult ->
                match targets with             
                | leftTarget, Some rightValue ->
                    let (replaced, rightResult) = addLeft rightValue right
                    if replaced then
                        Some (leftTarget, None), Pair (leftResult,rightResult)
                    else
                        (Some targets), Pair (leftResult, right)
                | _ ->
                    (Some targets), Pair (leftResult, right)
            | None, leftResult ->
                let targets, rightResult = explodeRec right (s + 1)
                match targets with
                | Some (Some leftValue, rightTarget) ->
                    let (replaced, leftResult) = addLeft leftValue left
                    if replaced then
                        Some (None, rightTarget), Pair (leftResult,rightResult)
                    else
                        (targets), Pair (leftResult, rightResult)
                | _ ->
                    (targets, Pair (leftResult, rightResult))    
                
    explodeRec n 0

let reduce ((left, right): SnailNumber) : SnailNumber =
    match tryExplode (Pair (left, right)) with
    | _, v ->
        match v with
        | Pair (left, right) -> (left,right)
        | _ -> failwith "no resulting pair"