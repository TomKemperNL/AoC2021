module AoC2021.Tests

open NUnit.Framework
open AoC2021.Day1

[<SetUp>]
let Setup () = ()

[<Test>]
let Day1APractice () =
    let input =
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

    let split = input.Split("\n") |> Array.toList
    let ints = List.map int split

    Assert.AreEqual(7, day1a ints)

[<Test>]
let Day1A () =
    let input =
        System.IO.File.ReadAllLines "./../../../Day1A.txt"

    let split = input |> Array.toList
    let ints = List.map int split

    Assert.AreEqual(1139, day1a ints)
