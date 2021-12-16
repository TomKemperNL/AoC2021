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
2311944581"""
        .Split(Environment.NewLine)
    |> Array.toList

[<Test>]
let Day15APractice () =
    Assert.AreEqual(40, day15a practiceInput)


[<Test>]
let Day15BPractice () =
    Assert.AreEqual(315, day15b practiceInput)


[<Test>]
let EdgeCases () =
    Assert.AreEqual(
        6,
        day15a [ "1111"
                 "1111"
                 "1111"
                 "1111" ]
    )

    Assert.AreEqual(
        6,
        day15a [ "1111"
                 "1111"
                 "1199"
                 "1111" ]
    )

    Assert.AreEqual(
        13,
        day15a [ "19999"
                 "11111"
                 "99111"
                 "11191"
                 "19999"
                 "11111" ]
    )

let input =
    (System.IO.File.ReadAllLines "./../../../Day15.txt")
    |> Array.toList


[<Test>]
let Day15A () =
    let result = day15a input
    Assert.Less(result, 459)
    Assert.AreEqual(456, result)
    


[<Test>]
let Day15B () =
    let result = day15b input
    Assert.AreEqual(456, result)


[<Test>]
let Increment () =
    let example = [|
        [| 1;2;3 |]
        [| 4;5;6 |]
        [| 7;8;9 |]
    |]
    
    let expected = [|
        [| 2;3;4 |]
        [| 5;6;7 |]
        [| 8;9;1 |]
    |]
    
    Assert.AreEqual(expected, Cave.incrementCave 1 example)
        
    let expected2 = [|
        [| 3;4;5 |]
        [| 6;7;8 |]
        [| 9;1;2 |]
    |]
    
    Assert.AreEqual(expected2, Cave.incrementCave 1 expected)    
    Assert.AreEqual(expected2, Cave.incrementCave 2 example)
    
    let expected3 = [|
        [|9;1;2|]
        [|3;4;5|]
        [|6;7;8|]
    |]
    
    Assert.AreEqual(expected3, Cave.incrementCave 8 example)

[<Test>]
let Combine () =
    let arrayarray : int [] [] = [| [| 1; 2 |]; [| 3; 4 |] |]

    let arrayarrayarrayarray : int [] [] [] [] =
        [| [| arrayarray; arrayarray |]
           [| arrayarray; arrayarray |] |]

    let expected : int [] [] =
        [| [| 1; 2; 1; 2 |]
           [| 3; 4; 3; 4 |]
           [| 1; 2; 1; 2 |]
           [| 3; 4; 3; 4 |] |]
    
    Assert.AreEqual(expected, Cave.combine arrayarrayarrayarray)
