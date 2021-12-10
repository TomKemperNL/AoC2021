module AoC2021.Tests.Day9

open System
open NUnit.Framework
open AoC2021.Day9


let practiceInputRaw = """2199943210
3987894921
9856789892
8767896789
9899965678"""

let practiceInput = practiceInputRaw.Split(Environment.NewLine) |> Array.toList

[<Test>]
let Day9APractice () =
    Assert.AreEqual(15, day9a practiceInput)
    
[<Test>]    
let GetHeight () =
    let heightMap = parse practiceInput
    Assert.AreEqual(2, getHeight heightMap (0,0))
    Assert.AreEqual(9, getHeight heightMap (2,0))
    Assert.AreEqual(9, getHeight heightMap (0,2))
    Assert.AreEqual(7, getHeight heightMap (3,3))
    

let input =
    (System.IO.File.ReadAllLines "./../../../Day9.txt") |> Array.toList
        
    
[<Test>]
let Day9A () =
    Assert.AreEqual(580, day9a input)
    
[<Test>]
let Day9BPractice () =
    Assert.AreEqual(1134, day9b practiceInput)
    
[<Test>]
let Day9B () =
    let result = day9b input
    Assert.AreEqual(856716, result)