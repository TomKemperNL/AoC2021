module AoC2021.Day18

open System

type Value =
    | Raw of int
    | Pair of Value * Value
    override this.ToString() =
        match this with
        | Raw x -> x.ToString()
        | Pair (left, right) ->
            let rec valueToString (v: Value) : string =
                match v with
                | Raw x -> x.ToString()
                | Pair (left, right) -> sprintf "[%s,%s]" (valueToString left) (valueToString right)

            sprintf "[%s,%s]" (valueToString left) (valueToString right)


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


type ExplodeProgress = (int option * int option) option

let rec addLeft (value: int) (n: Value) : bool * Value =
    match n with
    | Raw x -> true, Raw(x + value)
    | Pair (l, r) ->
        let success, lresult = addLeft value l
        success, Pair(lresult, r)


let rec addRight (value: int) (n: Value) : bool * Value =
    match n with
    | Raw x -> true, Raw(x + value)
    | Pair (l, r) ->
        let success, rresult = addRight value r
        success, Pair(l, rresult)

let tryExplode (n: SnailNumber) : (bool * SnailNumber) =
    let rec explodeRec nr steps : ExplodeProgress * Value =
        match nr, steps with
        | Raw n, _ -> None, Raw n
        | Pair (left, right), 4 ->
            match left, right with
            | Raw n, Raw m -> Some(Some n, Some m), Raw 0
            | _ -> failwith "Exploding pairs will always consist of two regular numbers?"
        | Pair (left, right), s ->
            let tryLeft = explodeRec left (s + 1)

            match tryLeft with
            | Some targets, leftResult ->
                match targets with
                | leftTarget, Some rightValue ->
                    let (replaced, rightResult) = addLeft rightValue right

                    if replaced then
                        Some(leftTarget, None), Pair(leftResult, rightResult)
                    else
                        (Some targets), Pair(leftResult, right)
                | _ -> (Some targets), Pair(leftResult, right)
            | None, leftResult ->
                let targets, rightResult = explodeRec right (s + 1)

                match targets with
                | Some (Some leftValue, rightTarget) ->
                    let (replaced, leftResult) = addRight leftValue left

                    if replaced then
                        Some(None, rightTarget), Pair(leftResult, rightResult)
                    else
                        (targets), Pair(leftResult, rightResult)
                | _ -> (targets, Pair(leftResult, rightResult))

    let progress, resultingValue = explodeRec (Pair n) 0

    match progress, resultingValue with
    | None, Pair (left, right) -> false, (left, right)
    | Some targets, Pair (left, right) -> true, (left, right)
    | _ -> failwith "no resulting pair"

let trySplit (nr: SnailNumber) : bool * SnailNumber =
    let rec splitRec (n: Value) : bool * Value =
        match n with
        | Raw x when x >= 10 ->
            let result =
                x
                |> float
                |> (fun x -> x / float (2))
                |> (fun x -> Math.Floor x, Math.Ceiling x)
                |> Pair.map int
                |> Pair.map Raw

            true, Pair result
        | Raw x -> false, Raw x
        | Pair (left, right) ->
            match splitRec left with
            | true, result -> true, Pair(result, right)
            | false, leftResult ->
                let tryRight, rightResult = splitRec right
                (tryRight, Pair(leftResult, rightResult))

    let changed, result = splitRec (Pair nr)

    match result with
    | Pair (left, right) -> changed, (left, right)
    | _ -> failwith "no resulting pair"

let reduce ((left, right): SnailNumber) : SnailNumber =
    let rec keepReducing n : SnailNumber =
        match tryExplode n with
        | true, result -> keepReducing result
        | false, result ->
            match trySplit n with
            | true, result -> keepReducing result
            | false, result -> result

    keepReducing (left, right)

let add (left: SnailNumber) (right: SnailNumber) : SnailNumber = (Pair left, Pair right) |> reduce

let addWithoutReduce (left: SnailNumber) (right: SnailNumber) : SnailNumber = (Pair left, Pair right)

let sum (nrs: SnailNumber list) : SnailNumber =
    match nrs with
    | [] -> failwith "uuuuuh, unspecified"
    | h :: t -> List.fold add h t


let magnitude ((left, right): SnailNumber) =
    let rec magnitudeRec (v: Value) : int =
        match v with
        | Raw n -> n
        | Pair (left, right) ->
            ((magnitudeRec left) * 3)
            + ((magnitudeRec right) * 2)

    ((magnitudeRec left) * 3)
    + ((magnitudeRec right) * 2)

let day18a (input: string list) =
    List.map fromString input
    |> sum
    |> reduce
    |> magnitude


let day18b (input: string list) =
    let inputs = List.map fromString input

    List.allPairs inputs inputs
    |> List.filter (fun (x, y) -> x <> y)
    |> List.map (fun (x, y) -> add x y)
    |> List.map magnitude
    |> List.max
