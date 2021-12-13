module AoC2021.Day13

open System
open System.Text.RegularExpressions

module Folding =
    type Fold =
        | AlongX of int
        | AlongY of int

    type Problem =
        { Coordinates: Set<int * int>
          Folds: Fold list }

    let (|DotPosition|_|) line =
        let m = Regex.Match(line, "(\d+),(\d+)")

        if m.Success then
            (m.Groups.[1].Value, m.Groups.[2].Value)
            |> Pair.map int
            |> Some
        else
            None

    let (|FoldInstruction|_|) line =
        let m =
            Regex.Match(line, "fold along (\w)=(\d+)")

        if m.Success then
            match m.Groups.[1].Value with
            | "x" -> Some(int m.Groups.[2].Value |> AlongX)
            | "y" -> Some(int m.Groups.[2].Value |> AlongY)
            | _ -> sprintf "Wut %s" line |> failwith
        else
            None

    let parse (input: string list) : Problem =
        let parseLine problem line =
            match line with
            | "" -> problem
            | DotPosition (x, y) ->
                { problem with
                      Coordinates = Set.add (x, y) problem.Coordinates }
            | FoldInstruction i ->
                { problem with
                      Folds = i :: problem.Folds }
            | _ -> sprintf "Cannot parse %s " line |> failwith

        let problem = List.fold parseLine { Coordinates = Set.empty; Folds = [] } input
        //Uuurgh
        { problem with Folds = List.rev problem.Folds }
        

    let fold (coords: Set<int*int>) (foldinstruction: Fold) =       
        match foldinstruction with
        | AlongX xf ->            
            let foldAlongX (x,y) =
                if x < xf then (x,y)
                else
                    let xDistance = x - xf
                    let newX = xf - xDistance
                    (newX, y)
            Set.map foldAlongX coords 
        | AlongY yf ->
            let foldAlongY (x,y) =
                if y < yf then (x,y)
                else
                    let yDistance = y - yf
                    let newY = yf - yDistance
                    (x, newY)
            Set.map foldAlongY coords            

let draw (points: Set<int*int>) =
    let (maxX, _) = Seq.maxBy fst points
    let (_, maxY) = Seq.maxBy snd points
    //rrrreaaally shitty drawing
    
    for y in 0 .. maxY do
        for x in 0 .. maxX do
            if Set.contains (x,y) points then
                printf "#"        
            else
                printf " "
        printfn ""
            
    
let day13a (input: string list) =
    let problem = Folding.parse input
    let firstFold :: _ = problem.Folds
    let result = Folding.fold problem.Coordinates firstFold
    Set.count result
    
    
let day13b (input: string list) =
    let problem = Folding.parse input
    let endCoords = List.fold Folding.fold problem.Coordinates problem.Folds
    draw endCoords
    //FPEKBEJL