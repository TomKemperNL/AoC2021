module AoC2021.Tests.Day16

open NUnit.Framework
open AoC2021.Day16

[<Test>]
let TestToDecimal () =
    let input = "D2FE28"
    Assert.AreEqual("110100101111111000101000", (Bits.fromHexString >> Bits.toBinaryString) input)