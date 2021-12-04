module AoC2021.Tests.Day4

open NUnit.Framework
open AoC2021.Day4


[<Test>]
let CanDetermineWinner () = 
    let mutable board = [|
        [| (1, false); (2, false); (3, false); |]
        [| (6, false); (5, false); (4, false); |]
        [| (7, false); (8, false); (9, false); |]
    |]

    Assert.AreEqual(false, isWinner board)
    board.[0].[0] <- (1, true)
    board.[0].[1] <- (2, true)
    board.[0].[2] <- (3, true)
    Assert.AreEqual(true, isWinner board)
    board.[0].[0] <- (1, false)
    board.[0].[1] <- (2, false)
    board.[0].[2] <- (3, false)
    Assert.AreEqual(false, isWinner board)
    board.[0].[1] <- (1, true)
    board.[1].[1] <- (2, true)
    board.[2].[1] <- (3, true)
    Assert.AreEqual(true, isWinner board)




[<Test>]
let Day4APractice() =
    let input =
        System.IO.File.ReadAllLines "./../../../Day4Practice.txt" |> Array.toList

    Assert.AreEqual(4512, day4a input)


[<Test>]
let Day4A() =
    let input =
        System.IO.File.ReadAllLines "./../../../Day4.txt" |> Array.toList

    Assert.AreEqual(69579, day4a input)
