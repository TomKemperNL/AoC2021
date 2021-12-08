module AoC2021.Tests.Day7


open NUnit.Framework
open AoC2021.Day7
open System.IO
open System

let practiceInput = "16,1,2,0,4,2,7,1,2,14".Split(",") |> Array.map int |> Array.toList

[<Test>]
let Day7APractice () =
    Assert.AreEqual((2, 37), day7a practiceInput)
    
let input =
    (System.IO.File.ReadAllText "./../../../Day7.txt").Split(",") |> Array.toList |> List.map int
  
[<Test>]
let Day7A () =    
    Assert.AreEqual((372, 337488), day7a input)