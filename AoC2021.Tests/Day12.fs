module AoC2021.Tests.Day12

open System
open NUnit.Framework
open AoC2021.Day12

let practiceInput1 =
    """start-A
start-b
A-c
A-b
b-d
A-end
b-end""".Split(Environment.NewLine) |> Array.toList

let practiceInput2 =
    """dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc""".Split(Environment.NewLine) |> Array.toList

let practiceInput3 =
    """fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW""".Split(Environment.NewLine) |> Array.toList

[<Test>]
let Day12APractice () =
    Assert.AreEqual(10, day12a practiceInput1)
    Assert.AreEqual(19, day12a practiceInput2)
    Assert.AreEqual(226, day12a practiceInput3)
    

[<Test>]
let Day12BPractice () =
    Assert.AreEqual(36, day12b practiceInput1)
    Assert.AreEqual(103, day12b practiceInput2)
    Assert.AreEqual(3509, day12b practiceInput3)


let input =
    """HF-qu
end-CF
CF-ae
vi-HF
vt-HF
qu-CF
hu-vt
CF-pk
CF-vi
qu-ae
ae-hu
HF-start
vt-end
ae-HF
end-vi
vi-vt
hu-start
start-ae
CS-hu
CF-vt""".Split(Environment.NewLine) |> Array.toList

[<Test>]
let Day12A () =
    Assert.AreEqual(3802, day12a input)
    

[<Test>]
let Day12B () =
    Assert.AreEqual(99448, day12b input)