module AoC2021.Day12

type Cave =
    | Start
    | End
    | Small of string
    | Large of string

type Caves = Map<Cave, Set<Cave>>

type Path = Cave list

module Caves =
    let findPaths (caves: Caves) =
        let isSmall c =
            match c with
            | Small _ -> true
            | _ -> false
        
        let rec findPath (found: Path list) (path: Cave list) : Path list =
            let options : Cave list =
                match path with
                | [] -> Map.find Start caves |> Set.toList
                | p :: _ ->
                    match Map.tryFind p caves with
                    | None -> []
                    | Some opts ->
                        let visitedSmalls = List.filter isSmall path |> Set.ofList |> Set.add Start
                        Set.difference opts visitedSmalls |> Set.toList 

            match path, options with
            | End :: _, _ ->
                path :: found
            | _, [] -> found            
            | _, _ ->
                let step opt = findPath found (opt :: path)
                List.collect step options

        findPath [] [Start]

    let parse (lines: string list) =
        let parseCave (cave: string) =
            match cave with
            | "start" -> Start
            | "end" -> End
            | name when name.ToUpper() = name -> Large name
            | name when name.ToLower() = name -> Small name
            | _ -> failwith (sprintf "no idea what to do with %s" cave)

        let parseLine (line: string) : Cave * Cave =
            match line.Split("-") with
            | [| left; right |] ->
                let c1 = parseCave left
                let c2 = parseCave right
                c1, c2
            | _ -> failwith (sprintf "no idea what to do with %s" line)

        let pairs : ((Cave * Cave) list) = List.map parseLine lines
        let flippedPairs = List.map (fun (a,b)-> (b,a)) pairs

        let addPath (caves: Caves) (c1, c2) : Caves =
            match Map.tryFind c1 caves with
            | None -> Map.add c1 (Set.singleton c2) caves
            | Some (connected) ->
                Map.add c1 (Set.add c2 connected) caves

        List.fold addPath Map.empty (List.append pairs flippedPairs)


let day12a (input: string List) =
    let caves = Caves.parse input
    let paths = Caves.findPaths caves
    List.length paths

let day12b (input: string List) =
    42