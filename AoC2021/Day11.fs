module AoC2021.Day11

type Octopus = int
type OctopusGrid = Octopus [] []

module OctopusGrid =
    let iter (fn: ((int * int) * Octopus) -> Octopus) (octopussies: OctopusGrid) =
        for y in 0 .. octopussies.Length - 1 do
            for x in 0 .. octopussies.[y].Length - 1 do
                let res = fn ((x, y), octopussies.[y].[x])
                octopussies.[y].[x] <- res

let parse (input: string list) : OctopusGrid =
    let parseLine (line: string) =
        line.ToCharArray() |> Array.map (string >> int)

    List.map parseLine input |> List.toArray

let getEnergy (octopussies: OctopusGrid) (x, y) = octopussies.[y].[x]

let increaseEnergy (octopussies: OctopusGrid) =
    OctopusGrid.iter (fun (_, v) -> v + 1) octopussies

let neighbours (grid: OctopusGrid) (x, y) =
    let maxY = Array.length grid
    let maxX = Array.length grid.[0]

    seq {
        for dx in [ -1; 0; 1 ] do
            for dy in [ -1; 0; 1 ] do
                if x <> 0 || x <> 0 then
                    if (x + dx) > 0 && (x + dx < maxX) then
                        if (y + dy) > 0 && (y + dy < maxY) then
                            yield (x + dx, y + dy)
    }
    |> Seq.toList

let flash (octopussies: OctopusGrid) =
    let neighbours = neighbours octopussies
    let getEnergy = getEnergy octopussies

    for y in 0 .. octopussies.Length - 1 do
        for x in 0 .. octopussies.[y].Length - 1 do
            if getEnergy (x, y) >= 9 then
                for (nx, ny) in neighbours (x, y) do
                    octopussies.[ny].[nx] <- octopussies.[ny].[nx] + 1

let day11a (input: string list) =
    let grid = parse input

    42
