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
