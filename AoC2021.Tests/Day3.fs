module AoC2021.Tests.Day3

open NUnit.Framework
open AoC2021.Day3

let practiceInputRaw = """00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010"""

[<Test>]
let TestBinary () =
    Assert.AreEqual(0, (toDecimal [|0;0;0|]))
    Assert.AreEqual(1, (toDecimal [|0;0;1|]))
    Assert.AreEqual(2, (toDecimal [|0;1;0|]))
    Assert.AreEqual(3, (toDecimal [|0;1;1|]))
    Assert.AreEqual(4, (toDecimal [|1;0;0|]))

[<Test>]
let Day3APractice () =
    printfn "%s" practiceInputRaw
    
    let practiceInput = practiceInputRaw.Split(System.Environment.NewLine) |> Array.toList    
    Assert.AreEqual(198, day3a practiceInput)

[<Test>]
let Day3BPractice () =
    let practiceInput = practiceInputRaw.Split(System.Environment.NewLine) |> Array.toList    
    Assert.AreEqual(230, day3b practiceInput)
  

[<Test>]
let Day3A() =

    let input =
        System.IO.File.ReadAllLines "./../../../Day3.txt" |> Array.toList

    Assert.AreEqual(3242606, day3a input)

[<Test>]
let Day3B() =
    let input =
        System.IO.File.ReadAllLines "./../../../Day3.txt" |> Array.toList

    Assert.AreEqual(4856080, day3b input)