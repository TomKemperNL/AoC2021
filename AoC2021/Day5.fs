module AoC2021.Day5

open System
open System.Text.RegularExpressions

type Coord = (int * int)
type Line = (Coord * Coord)


let parseLine line = 
    try 
        let m = Regex.Match(line, "(\\d+),(\\d+)\\s->\\s(\\d+),(\\d+)")
    
        let parseGroup (n: int) = 
            int(m.Groups.[n].Value)
        ((parseGroup 1, parseGroup 2), (parseGroup 3, parseGroup 4))
    with  
    | _ -> failwith (sprintf "Unable to parse %s" line)

let isOnLine line (x,y) =     
    match line with
    | ((x1,y1),(x2,y2)) when x1 = x2 -> 
        x = x1 && y >= (min y1 y2) && y <= (max y1 y2)
    | ((x1,y1),(x2,y2)) when y1 = y2 -> 
        y = y1 && x >= (min x1 x2) && x <= (max x1 x2)
    | _ -> failwith (sprintf "not a horizontal or vertical line %A" line)

let points line =     
    match line with
    | ((x1,y1),(x2,y2)) when x1 = x2 -> 
        seq {(min y1 y2) .. (max y1 y2)} |> Seq.map (fun y -> (x1, y))
    | ((x1,y1),(x2,y2)) when y1 = y2 -> 
        seq {(min x1 x2) .. (max x1 x2)} |> Seq.map (fun x -> (x, y1))        

let intersects line1 line2 =
    //damnit hier is een truukje voor...
    Set.intersect (Set.ofSeq (points line1)) (Set.ofSeq (points line2))


let day5a (input: string list) =
    let lines = List.map parseLine input
    let isApplicable ((x1,y1), (x2,y2)) = 
        x1 = x2 || y1 = y2
    let applicableLines = List.filter isApplicable lines
    let allPoints = Seq.collect points applicableLines
    Seq.countBy id allPoints |> Seq.filter (fun (p, occ) -> occ > 1) |> Seq.length

let day5b (input: string list) = 
    let lines = List.map parseLine input
    let allPoints = Seq.collect points lines
    Seq.countBy id allPoints |> Seq.filter (fun (p, occ) -> occ > 1) |> Seq.length
    