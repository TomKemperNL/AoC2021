module AoC2021.Tests.Day1

open NUnit.Framework
open AoC2021.Day1

let practiceInput =
        """199
200
208
210
200
207
240
269
260
263"""

let practiceInts = practiceInput.Split("\n") |> Array.toList |> List.map int

[<SetUp>]
let Setup () = ()

[<Test>]
let Day1APractice () =
    Assert.AreEqual(7, day1a practiceInts)

[<Test>]
let Day1BPractice () =
    Assert.AreEqual(5, day1b practiceInts)

let input =
        System.IO.File.ReadAllLines "./../../../Day1A.txt"

let ints =  input |> Array.toList |> List.map int

[<Test>]
let Day1A () =
    Assert.AreEqual(1139, day1a ints)

[<Test>]
let Day1B () =
    Assert.AreEqual(1103, day1b ints)