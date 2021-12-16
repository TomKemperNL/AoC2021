module AoC2021.Day15

type Cave = int [] []
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


    let analyseCave (cave: Cave) (finish: int * int) : Cave =
        let height = cave.Length
        let width = cave.[0].Length
        
        let rec findTotalRisk =
            Functions.memoize
                (fun (x, y) ->
                    let risk = cave.[y].[x]

                    if (x, y) = finish then
                        risk
                    else
                        let neighbours = neighbours cave [Down; Right;] (x, y)
                        
                        let smallestNeighbour =
                            List.map findTotalRisk neighbours |> List.min
                        risk + smallestNeighbour)

        let initRow y =
            Array.init width (fun x -> findTotalRisk (x, y))
        Array.init height initRow


let day15a (input: string list) =
    let cave = Cave.parse input
    let height = cave.Length
    let width = cave.[0].Length

    let riskCave = Cave.analyseCave cave (width - 1, height - 1)
    riskCave.[0].[0] - cave.[0].[0]
