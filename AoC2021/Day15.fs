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
        let neighbours = neighbours cave Direction.all

        let scratchPad =
            Array.init height (fun _ -> Array.init width (fun _ -> -1))

        scratchPad.[height - 1].[width - 1] <- cave.[height - 1].[width - 1]
        let xf, yf = finish
        scratchPad.[yf].[xf] <- cave.[yf].[xf]

        let rec analyseRec current (unvisited: Set<int * int>) (tentatives: int [] []) =
            let cx, cy = current
            let currentValue = tentatives.[cy].[cx]

            if Set.isEmpty unvisited then
                tentatives
            else
                let nbs = neighbours current |> Set.ofList
                let unvisitedNbs = Set.intersect nbs unvisited

                let updateTentative (x, y) =
                    let currentTentative = tentatives.[y].[x]
                    let cost = cave.[y].[x]
                    let newTentative = cost + currentValue

                    match currentTentative with
                    | -1 -> tentatives.[y].[x] <- newTentative
                    | oldEstimate when oldEstimate < newTentative -> tentatives.[y].[x] <- oldEstimate
                    | _ -> tentatives.[y].[x] <- newTentative

                //Set distances
                Set.iter updateTentative unvisitedNbs

                let unvisited = Set.remove current unvisited

                let findTentativeDistance (x, y) = tentatives.[y].[x]

                //zeer placeholder:
                let nextCandidates =
                    Set.filter (fun (x, y) -> tentatives.[y].[x] <> -1) unvisited

                if Set.isEmpty nextCandidates then
                    tentatives
                else
                    let next =
                        Set.minBy findTentativeDistance nextCandidates

                    analyseRec next unvisited tentatives

        let unvisited =
            Set.ofSeq
            <| seq {
                for x in 0 .. (width - 1) do
                    for y in 0 .. (height - 1) do
                        yield (x, y)
               }

        analyseRec finish unvisited scratchPad


    let incrementCave n (cave: Cave) =
        let height = cave.Length
        let width = cave.[0].Length

        Array.init
            height
            (fun y ->
                Array.init
                    width
                    (fun x ->
                        let result = (cave.[y].[x] + n)

                        if result <= 9 then
                            result
                        else
                            result % 9 //dit moet eigenlijk met modulo...
                        ))


    let combine (square: 'a [] [] [] []) : 'a [] [] =
        let verticalBlocks = square.Length
        let horizontalBlocks = square.[0].Length
        let blockHeight = square.[0].[0].Length
        let blockWidth = square.[0].[0].[0].Length

        let initRow y =
            Array.init
                (horizontalBlocks * blockWidth)
                (fun x ->
                    let blockX, xInBlock = (x / blockWidth), (x % blockWidth)
                    let blockY, yInBlock = (y / blockHeight), (y % blockHeight)

                    square.[blockY].[blockX].[yInBlock].[xInBlock])

        Array.init (verticalBlocks * blockHeight) initRow




let day15a (input: string list) =
    let cave = Cave.parse input
    let height = cave.Length
    let width = cave.[0].Length

    let riskCave =
        Cave.analyseCave cave (width - 1, height - 1)

    riskCave.[0].[0] - cave.[0].[0]

let day15b (input: string list) =
    let cave = Cave.parse input

    let initRow y =
        Array.init 5 (fun x -> Cave.incrementCave (x + y) cave)

    let caveOfCaves = Array.init 5 initRow
    let flattened = Cave.combine caveOfCaves

    let height = flattened.Length
    let width = flattened.[0].Length

    let riskCave =
        Cave.analyseCave flattened (width - 1, height - 1)

    riskCave.[0].[0] - flattened.[0].[0]
