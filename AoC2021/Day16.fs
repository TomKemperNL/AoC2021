module AoC2021.Day16

open AoC2021.Bits
open System.Text

type Header = { Version: int; Type: int }

type LengthTypeId =
    | TotalBitLength
    | NrOfPackets

type Length = int
type Literal = int64

type Operation =
    | Sum
    | Product
    | Minimum
    | Maximum
    | GreaterThan
    | LessThan
    | EqualTo

type Content =
    | Literal of Literal
    | Operator of Operation * Operator

and Packet = Header * Content
and Operator = LengthTypeId * Length * Packet list


let parseHeader bits =
    let versionBits, typeBits = List.splitAt 3 bits

    { Version = Bits.toDecimal versionBits
      Type = Bits.toDecimal typeBits }


let parseLiteral (bits, consumed) : Literal * int * bool list =
    let rec parseLiteralRec (todo: bool list) (result: bool list, consumed, remaining) =
        match todo with
        | true :: tail ->
            let nr, rest = List.splitAt 4 tail
            parseLiteralRec rest (List.append result nr, consumed + 5, rest)
        | false :: tail ->
            let nr, rest = List.splitAt 4 tail
            (List.append result nr, consumed + 5, rest)

    let resultingBits, consumed, remaining = parseLiteralRec bits ([], consumed, [])
    (Bits.toLongDecimal resultingBits), consumed, remaining

let rec parseOperator (bits, outerconsumed) : Operator * int * bool list =
    let rec parsePacketsBits (todo: bool list) (bitsRemaining: int) (result: Packet list, consumed, leftOver) =
        match bitsRemaining with
        | 0 -> result, consumed, leftOver
        | n ->
            let extraPacket, bitsConsumed, leftOver = parsePacketRec (todo, 0)

            parsePacketsBits
                leftOver
                (n - bitsConsumed)
                (List.append result [ extraPacket ], bitsConsumed + consumed, leftOver)

    let rec parsePacketsNr (todo: bool list) (packetsRemaining: int) (result: Packet list, consumed, leftOver) =
        match packetsRemaining with
        | 0 -> result, consumed, leftOver
        | n ->
            let extraPacket, bitsConsumed, leftOver = parsePacketRec (todo, consumed)
            parsePacketsNr leftOver (n - 1) (List.append result [ extraPacket ], bitsConsumed, leftOver)

    match bits with
    | true :: tail ->
        let lengthBits, operatorBits = List.splitAt 11 tail
        let length = Bits.toDecimal lengthBits

        let packets, consumed, remaining =
            parsePacketsNr operatorBits length ([], 0, [])

        (NrOfPackets, length, packets), outerconsumed + consumed + 12, remaining
    | false :: tail ->
        let lengthBits, operatorBits = List.splitAt 15 tail
        let length = Bits.toDecimal lengthBits

        let packets, consumed, remaining =
            parsePacketsBits operatorBits length ([], 0, [])

        (TotalBitLength, length, packets), outerconsumed + consumed + 16, remaining

and parsePacketRec (bits, consumed) =
    let headerBits, bodyBits = List.splitAt 6 bits
    let header = parseHeader headerBits

    let content, consumed, remaining =
        match header.Type with
        | 4 ->
            let result, consumed, remaining = parseLiteral (bodyBits, (consumed + 6))
            Literal result, consumed, remaining
        | opcode ->
            let op = match opcode with
                        | 0 -> Sum
                        | 1 -> Product
                        | 2 -> Minimum
                        | 3 -> Maximum
                        | 5 -> GreaterThan
                        | 6 -> LessThan
                        | 7 -> EqualTo
            
            let result, consumed, remaining =
                parseOperator (bodyBits, (consumed + 6))            
            
            Operator (op, result), consumed, remaining

    (header, content), consumed, remaining

let parsePacket bits : Packet =
    let packet, bitsConsumed, bitsLeftover = parsePacketRec (bits, 0)
    packet

let rec versionSum (packet: Packet) =
    let header, content = packet

    match content with
    | Literal _ -> header.Version
    | Operator (op, (lt, l, packets)) -> header.Version + List.sumBy versionSum packets

let rec interpret (packet:Packet) =
    let _, content = packet
    
    match content with
    | Literal v ->
        v
    | Operator (Sum, (_, _, packets)) ->
        let values = List.map interpret packets
        List.sum values
    | Operator (Product, (_, _, packets)) ->
        let values = List.map interpret packets
        List.fold (*) 1 values
    | Operator (Minimum, (_, _, packets)) ->
        let values = List.map interpret packets
        List.min values
    | Operator (Maximum, (_, _, packets)) ->
        let values = List.map interpret packets
        List.max values
    | Operator (GreaterThan, (_, _, [p1;p2])) ->
        if (interpret p1) > (interpret p2) then 1 else 0        
    | Operator (LessThan, (_, _, [p1;p2])) ->
        if (interpret p1) < (interpret p2) then 1 else 0
    | Operator (EqualTo, (_, _, [p1;p2])) ->
        if (interpret p1) = (interpret p2) then 1 else 0

let day16a (input: string) =
    let bits = Bits.fromHexString input
    let packet = parsePacket bits
    versionSum packet

let day16b (input: string) =
    let bits = Bits.fromHexString input
    let packet = parsePacket bits
    interpret packet