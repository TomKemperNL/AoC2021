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
