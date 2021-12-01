module AoC2021.Tests

open NUnit.Framework
open AoC2021.Day1

[<SetUp>]
let Setup () =
    ()

[<Test>]
let Test1 () =
    Assert.AreEqual(42, hello())
