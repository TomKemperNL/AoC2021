module AoC2021.Tests.Day6

open NUnit.Framework
open AoC2021.Day6
open System.IO
open System

let practiceInput = "3,4,3,1,2".Split(",") |> Array.toList |> List.map int

[<Test>]
let Day6APractice () =    
    Assert.AreEqual(26, day6a 18 practiceInput)
    Assert.AreEqual(5934, day6a 80 practiceInput)

[<Test>]
let Day6BPractice () =        
    Assert.AreEqual(26984457539L, day6b 256 practiceInput)

let input =
    (System.IO.File.ReadAllText "./../../../Day6.txt").Split(",") |> Array.toList |> List.map int

[<Test>]
let SingleFishie () = 
    let fishie = singleFishie 6 8
    Assert.AreEqual(day6a 6 [6], fishie 6 6)
    Assert.AreEqual(day6a 7 [6], fishie 7 6)
    Assert.AreEqual(day6a 13 [6], fishie 13 6)
    Assert.AreEqual(day6a 14 [6], fishie 14 6)
    Assert.AreEqual(day6a 15 [6], fishie 15 6)
    Assert.AreEqual(day6a 16 [6], fishie 16 6)
    Assert.AreEqual(day6a 21 [6], fishie 21 6)
    Assert.AreEqual(day6a 26 [6], fishie 26 6)
    Assert.AreEqual(day6a 80 [6], fishie 80 6)

[<Test>]
let Day6A () = 
    Assert.AreEqual(355386, day6a 80 input)

