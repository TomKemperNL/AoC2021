module AoC2021.Tests.Day2

open NUnit.Framework
open AoC2021.Day2

let practiceInput =
        """forward 5
down 5
forward 8
up 3
down 8
forward 2""".Split("\n") |> Array.toList

[<SetUp>]
let Setup () = ()

[<Test>]
let ParseTests () =
    Assert.AreEqual(Down 5, parse "down 5")
    Assert.AreEqual(Forward 8, parse "forward 8")
    Assert.AreEqual(Up 3, parse "up 3")

[<Test>]
let Day2APractice () =
    Assert.AreEqual(150, day2a practiceInput)

[<Test>]
let Day2BPractice () =
    Assert.AreEqual(900, day2b practiceInput)

let input =
        System.IO.File.ReadAllLines "./../../../Day2.txt" |> Array.toList

[<Test>]
let Day2A () =
    Assert.AreEqual(2039912, day2a input)

[<Test>]
let Day2B () =
    Assert.AreEqual(1942068080, day2b input)
