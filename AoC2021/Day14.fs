module AoC2021.Day14

open System.Text.RegularExpressions


module Polymer =
    type Template = Template of char list
    type PairRule = (char * char) * char
    type PairMap = Map<char * char, char>

    let parse (input: string list) : Template * PairMap =
        let template =
            (List.item 0 input).ToCharArray()
            |> Array.toList
            |> Template

        let rules = List.skip 2 input

        let parseRule (line: string) : PairRule =
            let m = Regex.Match(line, "(\w\w) -> (\w)")

            if m.Success then
                let [| a; b |] = m.Groups.[1].Value.ToCharArray()
                let [| c |] = m.Groups.[2].Value.ToCharArray()
                (a, b), c
            else
                sprintf "Cannot parse %s" line |> failwith

        let parsedRules = List.map parseRule rules |> Map.ofSeq
        (template, parsedRules)

    let step (map: PairMap) (Template template) : Template =
        let applyRule (a, b) =
            match Map.tryFind (a, b) map with
            | Some v -> [ v; b ]
            | None -> [ b ]

        let applied : char list =
            List.windowed 2 template
            |> List.map (fun [ a; b ] -> applyRule (a, b))
            |> List.concat

        let first = List.head template

        Template(first :: applied)

let day14a (input: string list) =
    let (template, map) = Polymer.parse input

    let (Polymer.Template result) =
        Loops.repeat 10 (Polymer.step map) template

    let counts = List.countBy id result
    let max = List.maxBy snd counts |> snd
    let min = List.minBy snd counts |> snd
    max - min


let day14b (input: string list) =
    let (template, map) = Polymer.parse input

    let (Polymer.Template result) =
        Loops.repeat 40 (Polymer.step map) template

    42