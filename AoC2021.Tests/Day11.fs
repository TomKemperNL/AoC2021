module AoC2021.Tests.Day11

open System
open NUnit.Framework
open AoC2021.Day11

let practiceInput =
    """5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526""".Split(Environment.NewLine) |> Array.toList 

[<Test>]
let TestNeighbours () =
    let grid = [|
        [|2;2;2|]
        [|2;2;2|]
        [|2;2;2|]
    |]
    
    Assert.AreEqual(8, List.length (neighbours grid (1,1))) 

[<Test>]
let Day11APractice () =
    Assert.AreEqual(1656, day11a practiceInput)

let input = """5723573158
3154748563
4783514878
3848142375
3637724151
8583172484
7747444184
1613367882
6228614227
4732225334""".Split(Environment.NewLine) |> Array.toList


[<Test>]
let Day11A () =
    Assert.AreEqual(1785, day11a input)
    

[<Test>]
let Day11BPractice () =
    Assert.AreEqual(195, day11b practiceInput)
    

[<Test>]
let Day11B () =
    Assert.AreEqual(354, day11b input)