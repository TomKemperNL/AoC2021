module AoC2021.Tests.Day5

open NUnit.Framework
open AoC2021.Day5
open System.IO
open System

let practiceInputRaw = """0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2"""

let practiceInput = practiceInputRaw.Split(Environment.NewLine) |> Array.toList

[<Test>]
let Day5APractice () =
    Assert.AreEqual(5, day5a practiceInput)

[<Test>]
let Day5BPractice () =
    Assert.AreEqual(12, day5b practiceInput)

let input =
    System.IO.File.ReadAllLines "./../../../Day5.txt" |> Array.toList

[<Test>]
let Day5A () =
    Assert.AreEqual(5169, day5a input)

[<Test>]
let Day5B() =
    Assert.AreEqual(12, day5b input)
