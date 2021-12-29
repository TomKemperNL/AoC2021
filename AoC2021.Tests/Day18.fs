module AoC2021.Tests.Day18

open System

open NUnit.Framework
open AoC2021.Day18


[<Test>]
let TestAddition () =
    let left : SnailNumber = Raw 1, Raw 2
    let right : SnailNumber = (Pair (Raw 3, Raw 4)), Raw 5
    let expected : SnailNumber = (Pair (Raw 1, Raw 2)), (Pair (Pair (Raw 3, Raw 4), Raw 5))
    Assert.AreEqual(expected, add left right)
    
[<Test>]
let TestToString () =
    Assert.AreEqual("[5,3]", (Raw 5, Raw 3) |> toString)
    Assert.AreEqual("[[5,2],3]", (Pair (Raw 5, Raw 2), Raw 3) |> toString)
    

[<Test>]
let TestStringConversion () =
    for case in [
        "[5,3]"; "[[5,2],3]"; "[[1,2],[2,3]]"; "[[1,[2,4]],[[3,5],3]]"
        "[7,[6,[5,[4,[3,2]]]]]";"[[[[0,9],2],3],4]";"[[6,[5,[4,[3,2]]]],1]";"[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]";
        "[[[[[9,8],1],2],3],4]"; "[7,[6,[5,[7,0]]]]"; "[[6,[5,[7,0]]],3]"
        "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"
        "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"
    ] do
        Assert.AreEqual(case, case |> fromString |> toString)

[<Test>]
let TestFromString () =
    Assert.AreEqual((Raw 5, Raw 3), "[5,3]"  |> fromString)
    Assert.AreEqual((Pair (Raw 5, Raw 2), Raw 3), "[[5,2],3]" |> fromString)
    Assert.AreEqual((Raw 5, Pair(Raw 2, Raw 3)), "[5,[2,3]]" |> fromString)
    Assert.AreEqual((Pair (Raw 1, Raw 2), Pair(Raw 2, Raw 3)), "[[1,2],[2,3]]" |> fromString)
    Assert.AreEqual((Pair (Raw 1, Pair (Raw 2, Raw 4)), Pair(Pair (Raw 3, Raw 5), Raw 3)), "[[1,[2,4]],[[3,5],3]]" |> fromString)
    
    
[<Test>]
let TestExplode () =    
//    Assert.AreEqual("[[[[0,9],2],3],4]", (reduce ("[[[[[9,8],1],2],3],4]" |> fromString)) |> toString)
//    Assert.AreEqual("[7,[6,[5,[7,0]]]]", (reduce ("[7,[6,[5,[4,[3,2]]]]]" |> fromString)) |> toString)    
//    Assert.AreEqual("[[6,[5,[7,0]]],3]", (reduce ("[[6,[5,[4,[3,2]]]],1]" |> fromString)) |> toString)
//    
//    Assert.AreEqual("[[3,[2,[8,0]]],[9,[5,[7,0]]]]", (reduce ("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]" |> fromString)) |> toString)
    Assert.AreEqual("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]", (reduce ("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]" |> fromString)) |> toString) //one step, incorrect
    //Assert.AreEqual("[[3,[2,[8,0]]],[9,[5,[7,0]]]]", (reduce ("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]" |> fromString)) |> toString) two steps 
    