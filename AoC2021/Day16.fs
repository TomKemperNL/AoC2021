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
    (Bits.toDecimal resultingBits), consumed, remaining

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
        | _ ->
            let result, consumed, remaining =
                parseOperator (bodyBits, (consumed + 6))

            Operator result, consumed, remaining

    (header, content), consumed, remaining

let parsePacket bits : Packet =
    let packet, bitsConsumed, bitsLeftover = parsePacketRec (bits, 0)
    packet

let rec versionSum (packet: Packet) =
    let header, content = packet

    match content with
    | Literal _ -> header.Version
    | Operator (lt, l, packets) -> header.Version + List.sumBy versionSum packets

let day16a (input: string) =
    let bits = Bits.fromHexString input
    let packet = parsePacket bits
    versionSum packet
