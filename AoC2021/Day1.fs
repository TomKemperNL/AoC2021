namespace AoC2021

module Day1 =
    let day1a (input: int list) =
        List.windowed 2 input |> List.filter (fun [a;b] -> b > a) |> List.length

    let day1b (input: int list) =        
        List.windowed 3 input |> List.map List.sum |> day1a
