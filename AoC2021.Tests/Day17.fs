module AoC2021.Tests.Day17

open NUnit.Framework
open AoC2021.Day17

let input = "target area: x=150..193, y=-136..-86"
let exampleInput = "target area: x=20..30, y=-10..-5"


[<Test>]
let TestParse () =
    Assert.AreEqual(((150, -136), (193, -86)), parse input)
    Assert.AreEqual(((20, -10), (30, -5)), parse exampleInput)

[<Test>]
let TestShoot () =
    let area = parse exampleInput
    
    Assert.AreEqual((Hit, 3), shoot area (7,2))    
    Assert.AreEqual((Hit, 6), shoot area (6,3))
    Assert.AreEqual((Hit, 0), shoot area (9,0))
    Assert.AreEqual((Miss, 0), shoot area (17,-4))
    

[<Test>]
let Day17APractice () =
    let ((dx, dy), maxH) = day17a exampleInput

    Assert.AreEqual(45, maxH)
    Assert.AreEqual((6, 9), (dx, dy))



[<Test>]
let Day17A () =
    let ((dx, dy), maxH) = day17a input
    Assert.AreEqual(9180, maxH)