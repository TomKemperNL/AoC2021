module AoC2021.Tests.Day7


open NUnit.Framework
open AoC2021.Day7
open System.IO
open System

let practiceInput =
    "16,1,2,0,4,2,7,1,2,14".Split(",")
    |> Array.map int
    |> Array.toList

[<Test>]
let Day7APractice () =
    Assert.AreEqual((2, 37), day7a practiceInput)

[<Test>]
let Day7BPractice () =
    Assert.AreEqual((5, 168), day7b practiceInput)

[<Test>]
let Medians () =
    Assert.AreEqual(3, median [ 1; 2; 3; 4; 5 ])
    Assert.AreEqual(2, median [ 1; 2; 2; 2; 5 ])
    Assert.AreEqual(2, median [ 1; 2; 2; 5 ])
    Assert.AreEqual(2.5, median [ 1; 2; 3; 5 ])
    Assert.AreEqual(2, median [0; 1; 1; 2; 2; 2; 4; 7; 14; 16])

[<Test>]
let CrabDistance () =
    Assert.AreEqual(10, crabDistance 1 5)
    Assert.AreEqual(66, crabDistance 16 5)
    Assert.AreEqual(15, crabDistance 0 5)
    Assert.AreEqual(45, crabDistance 14 5)

let input =
    (System.IO.File.ReadAllText "./../../../Day7.txt")
        .Split(",")
    |> Array.toList
    |> List.map int

[<Test>]
let Day7A () =
    Assert.AreEqual((372, 337488), day7a input)


[<Test>]
let Day7B () =
    Assert.AreEqual((480, 89647695), day7b input)