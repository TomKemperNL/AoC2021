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
let TestFromString () =
    Assert.AreEqual((Raw 5, Raw 3), "[5,3]"  |> fromString)
    Assert.AreEqual((Pair (Raw 5, Raw 2), Raw 3), "[[5,2],3]" |> fromString)
    Assert.AreEqual((Raw 5, Pair(Raw 2, Raw 3)), "[5,[2,3]]" |> fromString)
    Assert.AreEqual((Pair (Raw 1, Raw 2), Pair(Raw 2, Raw 3)), "[[1,2],[2,3]]" |> fromString)
    Assert.AreEqual((Pair (Raw 1, Pair (Raw 2, Raw 4)), Pair(Pair (Raw 3, Raw 5), Raw 3)), "[[1,[2,4]],[[3,5],3]]" |> fromString)
    
    
[<Test>]
let TestExplode () =
    let input = Pair(Pair( Pair ( Pair(Raw 9, Raw 8), Raw 1), Raw 2), Raw 3), Raw 4
    let expected = Pair(Pair(Pair(Raw 0, Raw 9), Raw 2), Raw 3), Raw 4
    Assert.AreEqual(expected, reduce input) 