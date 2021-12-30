module AoC2021.Tests.Day18

open System

open NUnit.Framework
open AoC2021.Day18


[<Test>]
let TestAddition () =
    let left: SnailNumber = Raw 1, Raw 2
    let right: SnailNumber = (Pair(Raw 3, Raw 4)), Raw 5

    let expected: SnailNumber =
        (Pair(Raw 1, Raw 2)), (Pair(Pair(Raw 3, Raw 4), Raw 5))

    Assert.AreEqual(expected, add left right)

[<Test>]
let TestToString () =
    Assert.AreEqual("[5,3]", (Raw 5, Raw 3) |> toString)
    Assert.AreEqual("[[5,2],3]", (Pair(Raw 5, Raw 2), Raw 3) |> toString)


[<Test>]
let TestStringConversion () =
    for case in
        [ "[5,3]"
          "[[5,2],3]"
          "[[1,2],[2,3]]"
          "[[1,[2,4]],[[3,5],3]]"
          "[7,[6,[5,[4,[3,2]]]]]"
          "[[[[0,9],2],3],4]"
          "[[6,[5,[4,[3,2]]]],1]"
          "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"
          "[[[[[9,8],1],2],3],4]"
          "[7,[6,[5,[7,0]]]]"
          "[[6,[5,[7,0]]],3]"
          "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"
          "[[3,[2,[8,0]]],[9,[5,[7,0]]]]" ] do
        Assert.AreEqual(case, case |> fromString |> toString)

[<Test>]
let TestFromString () =
    Assert.AreEqual((Raw 5, Raw 3), "[5,3]" |> fromString)
    Assert.AreEqual((Pair(Raw 5, Raw 2), Raw 3), "[[5,2],3]" |> fromString)
    Assert.AreEqual((Raw 5, Pair(Raw 2, Raw 3)), "[5,[2,3]]" |> fromString)
    Assert.AreEqual((Pair(Raw 1, Raw 2), Pair(Raw 2, Raw 3)), "[[1,2],[2,3]]" |> fromString)

    Assert.AreEqual(
        (Pair(Raw 1, Pair(Raw 2, Raw 4)), Pair(Pair(Raw 3, Raw 5), Raw 3)),
        "[[1,[2,4]],[[3,5],3]]" |> fromString
    )


[<Test>]
let TestExplode () =
    Assert.AreEqual(
        "[[[[0,9],2],3],4]",
        (tryExplode ("[[[[[9,8],1],2],3],4]" |> fromString))
        |> snd
        |> toString
    )

    Assert.AreEqual(
        "[7,[6,[5,[7,0]]]]",
        (tryExplode ("[7,[6,[5,[4,[3,2]]]]]" |> fromString))
        |> snd
        |> toString
    )

    Assert.AreEqual(
        "[[6,[5,[7,0]]],3]",
        (tryExplode ("[[6,[5,[4,[3,2]]]],1]" |> fromString))
        |> snd
        |> toString
    )

    Assert.AreEqual(
        "[[3,[2,[8,0]]],[9,[5,[7,0]]]]",
        (tryExplode ("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]" |> fromString))
        |> snd
        |> toString
    )

    Assert.AreEqual(
        "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]",
        (tryExplode (
            "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"
            |> fromString
        ))
        |> snd
        |> toString
    )


[<Test>]
let TestSplit () =
    Assert.AreEqual("[[[[0,7],4],[[7,8],[0,13]]],[1,1]]", (trySplit ("[[[[0,7],4],[15,[0,13]]],[1,1]]" |> fromString)) |> snd |> toString)
    Assert.AreEqual("[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]", (trySplit ("[[[[0,7],4],[[7,8],[0,13]]],[1,1]]" |> fromString)) |> snd |> toString)
    
[<Test>]
let TestReduce () =
    Assert.AreEqual("[[3,[2,[8,0]]],[9,[5,[7,0]]]]", (reduce ("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]" |> fromString)) |> toString)
    Assert.AreEqual("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]", (reduce ("[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]" |> fromString)) |> toString)

[<Test>]
let TestAddAndReduce () =
    let result = add ("[[[[4,3],4],4],[7,[[8,4],9]]]" |> fromString) ("[1,1]" |> fromString)
    Assert.AreEqual("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]", result |> toString)

[<Test>]
let TestMagnitude () =
    Assert.AreEqual(143, magnitude <| fromString "[[1,2],[[3,4],5]]")
    Assert.AreEqual(1384, magnitude <| fromString "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")
    Assert.AreEqual(445, magnitude <| fromString "[[[[1,1],[2,2]],[3,3]],[4,4]]")
    Assert.AreEqual(791, magnitude <| fromString "[[[[3,0],[5,3]],[4,4]],[5,5]]")
    Assert.AreEqual(1137, magnitude <| fromString "[[[[5,0],[7,4]],[5,5]],[6,6]]")
    Assert.AreEqual(3488, magnitude <| fromString "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]")
    

[<Test>]
let TestSums1 () =
    let input = ["[1,1]";"[2,2]";"[3,3]";"[4,4]"] |> List.map fromString
    let expected = "[[[[1,1],[2,2]],[3,3]],[4,4]]"
    Assert.AreEqual(expected, sum input |> toString)


[<Test>]
let TestSums2 () =
    let input = ["[1,1]";"[2,2]";"[3,3]";"[4,4]";"[5,5]"] |> List.map fromString
    let expected = "[[[[3,0],[5,3]],[4,4]],[5,5]]"
    Assert.AreEqual(expected, sum input |> toString)


[<Test>]
let TestSums3 () =
    let input = ["[1,1]";"[2,2]";"[3,3]";"[4,4]";"[5,5]";"[6,6]"] |> List.map fromString
    let expected = "[[[[5,0],[7,4]],[5,5]],[6,6]]"
    Assert.AreEqual(expected, sum input |> toString)

let exampleInput =
    """[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
[7,[5,[[3,8],[1,4]]]]
[[2,[2,2]],[8,[8,1]]]
[2,9]
[1,[[[9,3],9],[[9,0],[0,7]]]]
[[[5,[7,4]],7],1]
[[[[4,2],2],6],[8,7]]""".Split(Environment.NewLine) |> Array.toList
//
//[<Test>]
//let MustPreReduce () =
//    //There's only one problem: snailfish numbers must always be reduced, and the process of adding two snailfish numbers can result in snailfish numbers that need to be reduced.
//    //Wat betekent dat...
//    
//    //dit is de uitkomst van een voorbeeld, maar deze kan verder gereduced worden toch?
//    let input = fromString "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]"
//    let result = reduce input
//    
//    Assert.AreNotEqual(input |> toString, result |> toString)
    

[<Test>]
let BLERGH () =
    let _, result = tryExplode <| fromString "[[[[4,0],[5,0]],[[[4,5],[2,6]],[9,5]]],[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]]"
    Assert.AreEqual("[[[[4,0],[5,4]],[[0,[7,6]],[9,5]]],[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]]", result |> toString)


[<Test>]
let Day18APracticeExample () =
    //FirstStep
    let result = addWithoutReduce ("[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]" |> fromString) ("[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]" |> fromString)
    Assert.AreEqual("[[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]],[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]]", result |> toString)
    
    let _, result = tryExplode result
    Assert.AreEqual("[[[[4,0],[5,0]],[[[4,5],[2,6]],[9,5]]],[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]]", result |> toString)
    let _, result = tryExplode result
    Assert.AreEqual("[[[[4,0],[5,4]],[[0,[7,6]],[9,5]]],[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]]", result |> toString)
    
    
    //Steps
    let result = add ("[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]" |> fromString) ("[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]" |> fromString)
    Assert.AreEqual("[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]", result |> toString, "Step 1")
    
    let result = add ("[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]" |> fromString) ("[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]" |> fromString)
    Assert.AreEqual("[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]", result |> toString, "Step 2")
    
    let result = add ("[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]" |> fromString) ("[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]" |> fromString)
    Assert.AreEqual("[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]", result |> toString, "Step 3")
    
    let result = add ("[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]" |> fromString) ("[7,[5,[[3,8],[1,4]]]]" |> fromString)
    Assert.AreEqual("[[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]", result |> toString, "Step 4")
    
    
    //Full example
    let input = exampleInput |> List.map fromString
    let expected = "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"
    Assert.AreEqual(expected, sum input |> toString, "Full Example")


let practiceInput =
    """[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]""".Split(Environment.NewLine) |> Array.toList
    
[<Test>]
let Day18APractice () =
    Assert.AreEqual(4140, day18a practiceInput)
    

let input =
    (System.IO.File.ReadAllLines "./../../../Day18.txt") |> Array.toList
    

[<Test>]
let Day18A () =
    Assert.AreEqual(3551, day18a input)
    

[<Test>]
let Day18BPractice () =
    Assert.AreEqual(3993, day18b practiceInput)
    


[<Test>]
let Day18B () =
    //4589 too high?
    Assert.AreEqual(4555, day18b input)    