module AoC2021.Tests.Day13

open System
open System.IO
open AoC2021.Day13
open NUnit.Framework

let practiceInput =
    """6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5""".Split(Environment.NewLine) |> Array.toList

[<Test>]
let Day13APractice () =
    Assert.AreEqual(17, day13a practiceInput)
    
let input =
    (System.IO.File.ReadAllLines "./../../../Day13.txt") |> Array.toList

[<Test>]
let Day13A () =
    Assert.AreEqual(701, day13a input)
    

[<Test>]
let Day13B () =
    Assert.AreEqual((), day13b input)