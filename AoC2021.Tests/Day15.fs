module AoC2021.Tests.Day15

open System
open NUnit.Framework
open AoC2021.Day15

let practiceInput =
    """1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581""".Split(Environment.NewLine) |> Array.toList

[<Test>]
let Day15APractice () =
    Assert.AreEqual(40, day15a practiceInput)

let input = (System.IO.File.ReadAllLines "./../../../Day15.txt") |> Array.toList
    

//[<Test>]
let Day15A () =
    Assert.AreEqual(40, day15a input)