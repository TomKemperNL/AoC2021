module AoC2021.Bits

open System.Text


module Bits =
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

    let private hexAlphabet =
        Seq.append (seq { '0' .. '9' }) (seq { 'A' .. 'F' })

    let private hexToDecimalMap =
        Map.ofSeq (hexAlphabet |> Seq.zip <| { 0 .. 15 })

    let private hexToBitsMap =
        Map.ofSeq (
            hexAlphabet |> Seq.zip
            <| [ [ false; false; false; false ]
                 [ false; false; false; true ]
                 [ false; false; true; false ]
                 [ false; false; true; true ]
                 [ false; true; false; false ]
                 [ false; true; false; true ]
                 [ false; true; true; false ]
                 [ false; true; true; true ]
                 [ true; false; false; false ]
                 [ true; false; false; true ]
                 [ true; false; true; false ]
                 [ true; false; true; true ]
                 [ true; true; false; false ]
                 [ true; true; false; true ]
                 [ true; true; true; false ]
                 [ true; true; true; true ] ]
        )

    let fromHexString (hex: string) : bool list =
        hex.ToCharArray()
        |> Array.toList
        |> List.map (fun c -> Map.find c hexToBitsMap)
        |> List.concat
