module AoC2021.Day16

open AoC2021.Bits
open System.Text

type Header = { Version: int; Type: int }
type LengthTypeId =
    | TotalBitLength
    | NrOfPackets
type Length = int
type Literal = int

type Content =
    | Literal of Literal
    | Operator of Operator
and Packet = Header * Content
and Operator = LengthTypeId * Length * Packet list


let parseHeader bits =
    let versionBits, typeBits = List.splitAt 3 bits
    { Version = Bits.toDecimal versionBits; Type = Bits.toDecimal typeBits }


let parseLiteral bits : Literal =    
    let rec parseLiteralRec (todo: bool list) (result: bool list) : bool list=  
        match todo with
        | true :: tail ->
            let nr, rest = List.splitAt 4 tail
            parseLiteralRec rest (List.append result nr)
        | false :: tail ->
            let nr, _ = List.splitAt 4 tail
            (List.append result nr)
        
    let resultingBits = parseLiteralRec bits []
    (Bits.toDecimal resultingBits)
    
let rec parseOperator bits: Operator =
    let rec parsePacketsBits (todo: bool list) (bitsRemaining: int) (result: Packet list) : Packet list =
        match bitsRemaining with
        | 0 -> result
        | n -> 
            let extraPacket, bitsConsumed, leftOver = parsePacketRec todo
            parsePacketsBits leftOver (n - bitsConsumed) (extraPacket :: result)
    
    let rec parsePacketsNr (todo: bool list) (packetsRemaining: int) (result: Packet list) : Packet list =
        match packetsRemaining with
        | 0 -> result
        | n -> 
            let extraPacket, _, leftOver = parsePacketRec todo
            parsePacketsNr leftOver (n - 1) (extraPacket :: result)
    
    match bits with
    | true :: tail ->
        let lengthBits, operatorBits = List.splitAt 15 tail
        let length = Bits.toDecimal lengthBits        
        NrOfPackets, length, parsePacketsNr operatorBits length []
    | false :: tail ->
        let lengthBits, operatorBits = List.splitAt 15 tail
        let length = Bits.toDecimal lengthBits
        TotalBitLength, length, parsePacketsBits operatorBits length []
        
and parsePacketRec bits =
    let headerBits, bodyBits = List.splitAt 6 bits
    let header = parseHeader headerBits
    let content = match header.Type with
                    | 4 ->                        
                        Literal (parseLiteral bodyBits)
                    | _ ->
                        Operator (parseOperator bodyBits)
    (header, content)

let parsePacket bits : Packet =
   let packet, bitsConsumed, bitsLeftover = parsePacketRec bits
   packet
   
let day16a (input: string) =
    let bits = Bits.fromHexString input
    42