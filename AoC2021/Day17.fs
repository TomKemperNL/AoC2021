module AoC2021.Day17

open System.Text.RegularExpressions

type Velocity = int*int

type Area = (int*int) * (int*int)

type OnTarget =
    | Hit
    | Miss
    

let inArea ((lx, ly), (hx, hy)) (x,y) =
    x >= lx && x <= hx && y >= ly && y <= hy

let step (dx, dy) (x,y) =
    let newX = x + dx
    let newY = y + dy
    let newDy = dy - 1
    let newDx = if dx > 0 then dx - 1 else if dx < 0 then dx + 1 else dx
    
    (newDx, newDy), (newX, newY)

let parse input =
    let m = Regex.Match(input, "target area: x=(\\d+)..(\\d+), y=(-?\\d+)..(-?\\d+)")
    let coords = (m.Groups.[1].Value, m.Groups.[3].Value), (m.Groups.[2].Value, m.Groups.[4].Value) 
    Pair.map (Pair.map int) coords
    

let shoot ((lx, ly), (hx, hy)) (dx, dy) =
    let isHit = inArea ((lx, ly), (hx, hy))
    
    let rec shootRec (x,y) (dx,dy) maxY =
        if isHit (x,y) then
            Hit, maxY
        else if y < ly || x > hx then
            Miss, maxY
        else
            let newMax = if y > maxY then y else maxY
            let newVel, newPos = step (dx, dy) (x,y)
            shootRec newPos newVel newMax
    
    shootRec (0,0) (dx,dy) 0
    


let day17a (input: string) =
    let area = parse input
    seq { for dx in 0..300  do
              for dy in 0..300 do
                  let hitormiss, maxH = shoot area (dx,dy)
                  if hitormiss = Hit then
                      yield (dx, dy), maxH } |> Seq.maxBy snd
    

let day17b (input: string) =
    let area = parse input
    seq { for dx in 0..300  do
              for dy in -300..300 do
                  let hitormiss, maxH = shoot area (dx,dy)
                  if hitormiss = Hit then
                      yield (dx, dy), maxH } |> Seq.length