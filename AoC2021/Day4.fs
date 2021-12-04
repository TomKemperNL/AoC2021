module AoC2021.Day4

open System
open System.Text.RegularExpressions

type Board = (int * bool)[][]

type Bingo = {
    numbers: int list;
    boards: Board list
}

let isWinner (board: Board) =
    let hasHorizontalWinner (b: Board) : bool = 
        let rowIsFull (row: (int * bool)[]) : bool = 
            Array.forall (fun (_, b) -> b) row 
        Array.exists rowIsFull b

    let hasVerticalWinner (b: Board) : bool =
        let transposed = (Array.transpose b)
        hasHorizontalWinner transposed

    (hasHorizontalWinner board) || (hasVerticalWinner board)

let scoreBoard (board: Board) = 
    let unmarked = seq {
        for i in 0 .. board.Length - 1 do
            for j in 0 .. board.[i].Length - 1 do
                let (v, m) = board.[i].[j]
                if not m then 
                    yield v
    }

    Seq.sum unmarked

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

    let numbersLine = List.head input
    let numbers = numbersLine.Split(",") |> Array.map int

    let boardLines = List.skip 2 input |> List.filter (fun s -> s.Length > 0)
    let splitBoardLines = List.chunkBySize 5 boardLines

    let boards = List.map parseBoard splitBoardLines 

    {
        numbers = numbers |> Array.toList;
        boards = boards
    }


let day4a (input: string list) = 
    let bingo = parseInput input

    let rec findWinner numbers = 
        match numbers with
        | (h :: t) -> 
            List.iter (markNr h) bingo.boards
            match List.tryFind isWinner bingo.boards with
            | Some b ->
                let winnerScore = scoreBoard b
                winnerScore * h 
            | None ->
                findWinner t
        | [] ->
            0

    (findWinner bingo.numbers)
        
        
