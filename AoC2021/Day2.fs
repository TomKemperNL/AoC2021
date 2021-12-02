namespace AoC2021

open System.Text.RegularExpressions

module Day2 =
    type Command = 
    |Forward of int
    |Down of int
    |Up of int

    let (|ParseCommand|) line = 
        let m = Regex.Match(line, "(\\w+)\\s(\\d+)")
        if m.Success then
            let direction = m.Groups.[1].Value
            let steps = m.Groups.[2].Value |> int
            match (direction, steps) with 
            | "forward", n -> Some (Forward n)
            | "up", n -> Some (Up n)
            | "down", n -> Some (Down n)
            | _, _ -> None
        else 
            None

    let parse (line: string)= 
        match line with
        | ParseCommand (Some result) -> result
        | ParseCommand None -> failwith <| sprintf "could not parse %s" line        

    let day2a input =     
        let step (hor, depth) command =
            match command with 
            | Forward n -> (hor + n, depth)
            | Up n -> (hor, depth - n)
            | Down n -> (hor, depth + n)

        let commands = List.map parse input
        let start = (0,0)
        let (endHor, endDepth) = List.fold step start commands
        endHor * endDepth

    let day2b input =     
        let step (hor, depth, aim) command =
            match command with 
            | Forward n -> 
                (hor + n, depth + (aim * n), aim)
            | Up n -> 
                (hor, depth, aim - n)
            | Down n -> 
                (hor, depth, aim + n)

        let commands = List.map parse input
        let start = (0,0,0)
        let (endHor, endDepth, endAim) = List.fold step start commands
        endHor * endDepth

