namespace AoC2021

open System.Collections.Generic

module Debug =
    let printArrays (itemPrint: 'a -> unit) (arrays: 'a [] []) =
        for y in 0 .. arrays.Length do
            for x in 0 .. arrays.[y].Length do
                itemPrint arrays.[y].[x]

            printfn ""

module Functions =
    let repeat (n: int) (fn: 'a -> 'a) : 'a -> 'a =
        List.init n (fun _ -> fn) |> List.reduce (>>)

    let memoize (fn: 'a -> 'b) =
        let cache = Dictionary<_, _>()

        let memoized (arg: 'a) =
            match cache.TryGetValue(arg) with
            | (true, v) -> v
            | (false, _) ->
                let (res: 'b) = fn arg
                cache.Add(arg, res)
                res

        memoized

module Pair =
    let map f (x, y) = (f x, f y)

module Set =

    let minBy fn (items: Set<'a>) : 'a =
        let start = None, None

        let isSmaller state item =
            match state with
            | None, None -> Some item, Some(fn item)
            | Some k, Some v ->
                let newV = fn item

                if newV < v then
                    Some item, Some newV
                else
                    Some k, Some v
            | s -> s

        let result = Set.fold isSmaller start items

        match result with
        | Some k, Some v -> k
        | _ -> failwith "Blergh"

module Bits =
    open System.Collections
    let toDecimal (bits: int seq) = 
        let reversed = Seq.rev bits
        let ixrev = Seq.indexed reversed
        let step (total: int) ((ix: int), (i: int)) = 
            total + (i * (pown 2 ix))
        Seq.fold step 0 ixrev