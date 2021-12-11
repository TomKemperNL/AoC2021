module AoC2021.Day11

type Octopus = (int * bool)
type OctopusGrid = Octopus [] []

module OctopusGrid =
    let iter (fn: (Octopus) -> Octopus) (octopussies: OctopusGrid) =
        for y in 0 .. octopussies.Length - 1 do
            for x in 0 .. octopussies.[y].Length - 1 do
                let res = fn octopussies.[y].[x]
                octopussies.[y].[x] <- res
                
    let print (octopussies: OctopusGrid) =
        let toCell (octopus: Octopus) : string =
            match octopus with
            | (e, true) -> sprintf "<%d>" e
            | (e, false) -> sprintf " %d " e
        
        for y in 0 .. octopussies.Length - 1 do            
            String.concat "" (Array.map toCell octopussies.[y]) |> printfn "%s"

let parse (input: string list) : OctopusGrid =
    let parseLine (line: string) =
        line.ToCharArray()
        |> Array.map (string >> int >> fun n -> (n, false))

    List.map parseLine input |> List.toArray

let getEnergy (octopussies: OctopusGrid) (x, y) = fst octopussies.[y].[x]

let increaseEnergy (octopussies: OctopusGrid) =
    OctopusGrid.iter (fun (e, flashed) -> (e + 1, flashed)) octopussies

let neighbours (grid: 'a[][]) (x, y) =
    let maxY = Array.length grid
    let maxX = Array.length grid.[0]

    seq {
        for dx in [ -1; 0; 1 ] do
            for dy in [ -1; 0; 1 ] do
                if dx <> 0 || dy <> 0 then
                    if (x + dx) >= 0 && ((x + dx) < maxX) then
                        if (y + dy) >= 0 && ((y + dy) < maxY) then
                            yield (x + dx, y + dy)
    }
    |> Seq.toList

let flash (octopussies: OctopusGrid) =
    let neighbours = neighbours octopussies
    let getEnergy = getEnergy octopussies

    let mutable keepFlashing = true
    let mutable flashes = 0
    
    while keepFlashing do
        keepFlashing <- false

        for y in 0 .. octopussies.Length - 1 do
            for x in 0 .. octopussies.[y].Length - 1 do
                let (energy, flashed) = octopussies.[y].[x]

                if getEnergy (x, y) > 9 && not flashed then
                    flashes <- flashes + 1
                    keepFlashing <- true
                    octopussies.[y].[x] <- (energy, true)

                    for (nx, ny) in neighbours (x, y) do
                        let (nenergy, nflashed) = octopussies.[ny].[nx]
                        octopussies.[ny].[nx] <- (nenergy + 1, nflashed)
    flashes

let reset (octopussies: OctopusGrid) =
    let resetOctopus (e, _) =
        if e > 9 then (0, false) else (e, false)
    OctopusGrid.iter resetOctopus octopussies

let step (octopussies: OctopusGrid) =
    increaseEnergy octopussies
    let flashes = flash octopussies
    reset octopussies
    flashes

let day11a (input: string list) =
    let grid = parse input
    let mutable totalFlashes = 0
    for i in 1..100 do
        totalFlashes <- totalFlashes + step grid
    totalFlashes
