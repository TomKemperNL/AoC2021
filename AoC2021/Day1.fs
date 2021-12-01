namespace AoC2021

module Day1 =
    let day1a (input: int list) =
        let increase (prev: int option, total: int) cur =
            match prev with
            | None -> (Some cur, total)
            | Some n ->
                if cur > n then
                    (Some cur, total + 1)
                else
                    (Some cur, total)

        let (_, total) = List.fold increase (None, 0) input
        total

    let toSlidingWindows3 items = 
        let count = List.length items
        let itemsSkip0 = List.take (count - 2) items
        let itemsSkip1 = List.skip 1 items |> List.take (count - 2)
        let itemsSkip2 = List.skip 2 items |> List.take (count - 2)

        List.map3 (fun x y z -> x + y + z) itemsSkip0 itemsSkip1 itemsSkip2

    let day1b (input: int list) = 
        let windowed = toSlidingWindows3 input
        day1a windowed

