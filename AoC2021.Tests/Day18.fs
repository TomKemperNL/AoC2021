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