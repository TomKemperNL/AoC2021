module AoC2021.Tests.Day14

open System
open AoC2021
open NUnit.Framework
open AoC2021.Day14

let practiceInput =
    """NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C""".Split(Environment.NewLine) |> Array.toList

[<Test>]
let Day14APractice () =
    Assert.AreEqual(1588, day14a practiceInput)


//[<Test>]
let Day14BPractice () =
    Assert.AreEqual(1588, day14b practiceInput)

let input =
    (System.IO.File.ReadAllLines "./../../../Day14.txt") |> Array.toList
    

[<Test>]
let Day14A () =
    Assert.AreEqual(3048, day14a input)
