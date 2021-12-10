module AoC2021.Tests.Day10

open System
open NUnit.Framework
open AoC2021.Day10

let practiceInput = """[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]""".Split(Environment.NewLine) |> Array.toList

[<Test>]
let Day10APractice () =
    Assert.AreEqual(26397, day10a practiceInput)
    

let input =
    (System.IO.File.ReadAllLines "./../../../Day10.txt") |> Array.toList

[<Test>]
let Day10A () =
    Assert.AreEqual(240123, day10a input)
    
[<Test>]
let Day10BPractice () =
    Assert.AreEqual(288957, day10b practiceInput)

[<Test>]
let Day10B () =    
    //too low 202784119
    Assert.AreNotEqual(202784119, day10b input)