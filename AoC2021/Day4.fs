module AoC2021.Day4

open System
open System.Text.RegularExpressions

type Board = (int * bool)[][]

type Bingo = {
    numbers: int list;
    boards: Board list
}

let isWinner (board: Board) =
    let hasHorizontalWinner = Array.exists (Array.forall snd)
    let hasVerticalWinner = Array.transpose >> hasHorizontalWinner
    (hasHorizontalWinner board) || (hasVerticalWinner board)

let visit2D (twoD: 't[][]) = 
    seq {
        for i in 0 .. twoD.Length - 1 do
            for j in 0 .. twoD.[i].Length - 1 do
                yield twoD.[i].[j]
    }

let scoreBoard (board: Board) =     
    visit2D board |> Seq.filter (snd >> not) |> Seq.map fst |> Seq.sum

let markNr nr (board: Board) : unit =     
    //To mutate, or not to mutate
    for i in 0 .. board.Length - 1 do
        for j in 0 .. board.[i].Length - 1 do
            let (v, m) = board.[i].[j]
            if v = nr then 
                board.[i].[j] <- (v, true)

let parseBoard (input: string list) =    
    let parseRow (row: string) = 
        try
            let nrs = Regex.Split(row.Trim(), "\\s+");
            Array.map int nrs |> Array.map (fun n -> (n, false))
        with 
        | ex -> raise (new Exception(sprintf "Error parsing %s" row, ex))
    Array.map parseRow (List.toArray input)

let parseInput (input: string list) =
    let boardsize = 5
    let lines = List.skip 2 input |> List.filter (fun s -> s.Length > 0)
    {
        numbers = (List.head input).Split(",") |> Array.map int |> Array.toList;
        boards = lines |> (List.chunkBySize boardsize) |> (List.map parseBoard)
    }

let day4a (input: string list) = 
    let bingo = parseInput input

    let rec findWinner numbers = 
        match numbers with
        | (h :: t) -> 
            List.iter (markNr h) bingo.boards
            match List.tryFind isWinner bingo.boards with
            | Some b -> (scoreBoard b) * h 
            | None -> findWinner t
        | [] -> failwith "No Bingo!"

    findWinner bingo.numbers

let day4b (input: string list) =
    let bingo = parseInput input
    
    let rec findWinner numbers (boards: Board list) result = 
        match (numbers, boards) with
        | (h :: t), (_::_) -> 
            List.iter (markNr h) boards

            let winners = List.filter isWinner boards
            let newBoards = List.except winners boards
            let newResults = List.map (fun b -> (b, h)) winners

            findWinner t newBoards (newResults @ result)
        | (_, []) ->
            List.head result
        | ([], _) ->
            List.head result
            
    let (b, nr) = findWinner bingo.numbers bingo.boards []
    let score = scoreBoard b
    nr * score
