module AoC2021.Day9

let parse (input: string list) : int [] [] =
    let parseLine (line: string) =
        line.ToCharArray() |> Array.map (string >> int)

    List.map parseLine input |> List.toArray

let neighbours (heightmap: int [] []) (x, y) =
    let maxX = heightmap.[0].Length - 1
    let maxY = heightmap.Length - 1

    let candidates =
        [ (x - 1, y)
          (x + 1, y)
          (x, y - 1)
          (x, y + 1) ]

    let inBounds (x, y) =
        x >= 0 && x <= maxX && y >= 0 && y <= maxY

    List.filter inBounds candidates

let getHeight (heightmap: int [] []) (x, y) = heightmap.[y].[x]

let isLowPoint (heightmap: int [] []) (x, y) =
    let neighbours = neighbours heightmap (x, y)

    let values =
        List.map (getHeight heightmap) neighbours

    let height = getHeight heightmap (x, y)
    List.forall ((<) height) values

let lowPoints (heightmap: int [] []) =
    seq {
        for y in 0 .. heightmap.Length - 1 do
            for x in 0 .. heightmap.[0].Length - 1 do
                yield (x, y)
    }
    |> Seq.filter (isLowPoint heightmap)

let findBasinNeighbours (heightmap: int [] []) (x, y) =
    let height = getHeight heightmap (x, y)

    let isBasinNeighbour (x, y) =
        let neighbourHeight = getHeight heightmap (x, y)
        neighbourHeight < 9 && neighbourHeight > height

    neighbours heightmap (x, y)
    |> List.filter isBasinNeighbour

let findBasin (heightmap: int [] []) lowPoint =
    let rec exploreBasin (edgesToExplore: Set<int * int>) (basin: Set<int * int>) =
        match edgesToExplore with
        | edges when (Set.count edges) = 0 -> basin
        | edges ->
            let newBasin = Set.union edges basin

            let newEdges =
                Set.map (fun p -> findBasinNeighbours heightmap p |> Set.ofList) edges
                |> Set.unionMany
            let newEdges = Set.difference newEdges basin

            exploreBasin newEdges newBasin

    exploreBasin (Set.singleton lowPoint) Set.empty



let day9a (input: string list) =
    let heightmap = parse input
    let points = lowPoints heightmap

    Seq.map (getHeight heightmap) points
    |> Seq.map ((+) 1)
    |> Seq.sum


let day9b (input: string list) =
    let heightmap = parse input
    let points = lowPoints heightmap |> Seq.toList
    let basins = List.map (findBasin heightmap) points

    let sizes =
        List.map Set.count basins
        |> List.sortDescending
        |> List.take 3

    List.fold (*) 1 sizes
