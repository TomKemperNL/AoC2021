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


let input =
    (System.IO.File.ReadAllText "./../../../Day6.txt").Split(",") |> Array.toList |> List.map int

[<Test>]
let Day6A () = 
    Assert.AreEqual(355386, day6a 80 input)