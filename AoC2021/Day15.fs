module AoC2021.Day15

type Cave = int [] []
type CaveAnalysis = int*int [] []
type Position = (int * int) * int

type Direction =
    | Up
    | Down
    | Left
    | Right

module Direction =
    let all = [ Up; Down; Left; Right ]

module Cave =
    let parse (input: string list) : int [] [] =
        let parseLine (line: string) =
            line.ToCharArray() |> Array.map (string >> int)

        List.map parseLine input |> List.toArray

    let totalRisk (path: Position list) = List.sumBy snd path

    let neighbours (cave: Cave) (directions: Direction list) (x, y) =
        let maxX = cave.[0].Length - 1
        let maxY = cave.Length - 1

        let neighbour dir =
            match dir with
            | Up -> x, y - 1
            | Down -> x, y + 1
            | Left -> x - 1, y
            | Right -> x + 1, y

        let inBounds (x, y) =
            x >= 0 && x <= maxX && y >= 0 && y <= maxY

        List.map neighbour directions
        |> List.filter inBounds


    let analyseCave cave finish =
        ()
    
    
    let findPath start finish cave : Position list =
        let optionsToFinish = neighbours cave [ Up; Left ]
        let optionsFromPos = neighbours cave [ Down; Right ]

        let rec findRec path target (x, y) =
            if (x, y) = target then
                path
            else
                let path = ((x, y), cave.[y].[x]) :: path

                optionsFromPos (x, y)
                |> List.map (findRec path target)
                |> List.minBy totalRisk

        findRec [] finish start

let day15a (input: string list) =
    let cave = Cave.parse input
    let width = cave.[0].Length
    let height = cave.Length

    let route =
        Cave.findPath (0, 0) (width - 1, height - 1) cave

    Cave.totalRisk route
