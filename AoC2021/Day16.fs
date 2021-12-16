module AoC2021.Day16

open System.Text

type Header = { Version: int; Type: int }
type LengthTypeId = LengthTypeId of bool
type Length = int
type Literal = int

type Content =
    | Literal of Literal
    | Operator of Operator
and Packet = Header * Content
and Operator = LengthTypeId * Length * Packet list

module Bits =

    open System.Collections

    let toDecimal (bits: bool list) =
        let reversed = List.rev bits
        let ixrev = List.indexed reversed

        let step (total: int) ((ix: int), (b: bool)) =
            let i = if b then 1 else 0
            total + (i * (pown 2 ix))

        List.fold step 0 ixrev

    let toBinaryString (bits: bool list) : string =
        let sb = StringBuilder()

        for b in bits do
            (if b then
                 sb.Append("1")
             else
                 sb.Append("0"))
            |> ignore

        sb.ToString()


    let private hexToDecimalMap =        
         Map.ofSeq (Seq.append (seq { '0' .. '9' }) (seq { 'A' .. 'F' }) |> Seq.zip <| { 0 .. 15 })  

    let fromHexString (hex: string) : bool list =


        let mapping =
            Map.ofList [

                          ]

        []
