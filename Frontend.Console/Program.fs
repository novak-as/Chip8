open System
open System.IO

let romName = "PONG"


let loadRom(filenname) : (byte[])[] = 

    seq {
        use reader = new BinaryReader(File.Open($"C:\\Users\\onovak\\Documents\\repos_personal\\chip8\\roms\\{filenname}", FileMode.Open, FileAccess.Read, FileShare.Read))
    
        while (reader.BaseStream.Position < reader.BaseStream.Length) do
            yield reader.ReadBytes(2)

    } |> Seq.toArray

type nible = byte
type trible = int16 //because why not, this is just full byte + nible

let rnd = Random()
let data = loadRom(romName)
let mutable programCounter:int16 = 0s
let mutable I:int16 = 0s
let variables: byte[] = Array.create 16 0uy
let memory: byte[] = Array.create 4096 0uy
let inputs:bool[] = Array.create 16 false
let stack:int16[] = Array.create 16 0s
let display: bool[,] = Array2D.create 64 32 false
let mutable delayTimer:byte = 0uy
let mutable soundTimer:byte = 0uy
let mutable stackPointer:byte = 0uy

let getLowNibble(octet):nible = 
    (byte)(octet &&& (byte)0x0F)

let getHightNibble(octet):nible = 
    (byte)(octet >>> 4)

let getNNN(octets: byte[]): trible =
    let low0 = getLowNibble(octets[0])
    BitConverter.ToInt16([| octets[1]; low0 |], 0)

let splitOctet(octet): nible*nible = 
    let highNibble = getHightNibble(octet)
    let lowNibble = getLowNibble(octet)

    (highNibble, lowNibble)

let op_0NNN (nnn: trible) = 
    stack[(int)stackPointer] <- programCounter - 1s
    stackPointer <- stackPointer + 1uy
    programCounter <- nnn

let op_00EE () = 
    stackPointer <- stackPointer - 1uy
    let addr = stack[(int)stackPointer]
    programCounter <- addr
    
let op_00E0 () = 
    for x in 0 .. display.GetLength(0) do
        for y in 0 .. display.GetLength(1) do
            display[x,y] <- false

let op_1NNN (nnn:trible) = 
    programCounter <- nnn / 2s

let op_3XNN (x:nible, nn:byte) = 
    if variables[(int)x] = nn then
        programCounter <- programCounter + 1s

let op_4XNN (x:nible, nn:byte) = 
    if variables[(int)x] <> nn then
        programCounter <- programCounter + 1s

let op_5XY0 (x:nible, y:nible) = 
    if variables[(int)x] = variables[(int)y] then
        programCounter <- programCounter + 1s

let op_6XNN (x:nible, nn:byte)=
    variables[(int)x] <- nn

let op_7XNN (x:nible, nn:byte)=
    variables[(int)x] <- variables[(int)x] + nn

let op_8XY0 (x:nible, y:nible) = 
    variables[(int)x] <- variables[(int)y]

let op_8XY1 (x:nible, y:nible) = 
    variables[(int)x] <- variables[(int)x] ||| variables[(int)y]

let op_8XY2 (x:nible, y:nible) = 
    variables[(int)x] <- variables[(int)x] &&& variables[(int)y]

let op_8XY3 (x:nible, y:nible) = 
    variables[(int)x] <- variables[(int)x] ^^^ variables[(int)y]

let op_8XY4 (x:nible, y:nible) = 
    variables[(int)x] <- variables[(int)x] + variables[(int)y]

let op_8XY5 (x:nible, y:nible) = 
    if variables[(int)y] > variables[(int)x] then 
        variables[15] <- 0uy
    else
        variables[15] <- 1uy

    variables[(int)x] <- variables[(int)x] - variables[(int)y]

let op_8XY6 (x:nible) =    
    variables[15] <- variables[(int)x] &&& 1uy
    variables[(int)x] <- variables[(int)x] >>> 1

let op_8XY7 (x:nible, y:nible) = 
    if variables[(int)x] > variables[(int)y] then
        variables[15] <- 0uy
    else
        variables[15] <- 1uy

    variables[(int)x] <- variables[(int)y] - variables[(int)x]

let op_8XYE (x:nible) = 
    variables[15] <- (variables[(int)x] &&& 0x80uy) >>> 7
    variables[(int)x] <- variables[(int)x] <<< 1


let op_ANNN (nnn:trible) = 
    I <- nnn

let op_BNNN (nnn:trible) = 
    programCounter <- (int16)(variables[0]) + nnn

let op_CXNN (x:nible, nn:byte)=
    let random = (byte)(rnd.Next(0,256))
    variables[(int)x] <- random &&& nn

let op_DXYN (x:byte, y:byte, n:byte)=

    let mutable screenUpdated = false

    for i in 0uy .. n do
        let row = memory[(int)(I+(int16)(i*8uy))]

        for pixel in 0uy .. 8uy do
            let bit = (row >>> (int)pixel) &&& 1uy

            if display[(int)(x+pixel), (int)(y+i)] <> (bit = 1uy) then
                screenUpdated <- true

            display[(int)(x+pixel), (int)(y+i)] <- (bit = 1uy)

    if screenUpdated then
        variables[15] <- 1uy
    else
        variables[15] <- 0uy
    
let op_EX9E (x:nible) =
    let key = inputs[(int)x]
    if key then
        programCounter <- programCounter+1s

let op_EXA1 (x:nible) =
    let key = inputs[(int)x]
    if not key then
        programCounter <- programCounter+1s

let op_FX07 (x:nible) = 
    variables[(int)x] <- delayTimer

let op_FX0A (x:nible) = 
    variables[(int)x] <- (byte)(Console.ReadKey().Key)

let op_FX15 (x:nible) = 
    delayTimer <- x

let op_FX18 (x:nible) = 
    soundTimer <- x

let op_FX1E (x:nible) = 
    I <- I + (int16)(variables[(int)x])

let op_FX33 (x:nible)=
    let value = variables[(int)x]

    memory[(int)I] <- value / 100uy
    memory[(int)(I+(int16)1uy)] <- (value % 100uy) / 10uy
    memory[(int)(I+(int16)2uy)] <- (value % 100uy) % 10uy

let op_FX55 (x:nible) =
    for i in 0uy .. x do
        memory[(int)(I+(int16)i)] <- variables[(int)i]

let op_FX65 (x:nible) =
    for i in 0uy .. x do
        variables[(int)i] <- memory[(int)(I+(int16)i)]

while true do

    let currentCommand = data.[(int)programCounter]

    programCounter <- programCounter + 1s

    let (firstHigh, firstLow) = splitOctet(currentCommand[0])
    let (secondHigh, secondLow) = splitOctet(currentCommand[1])

    try
        match (firstHigh, firstLow, secondHigh, secondLow) with
            | (0uy ,0uy, 0x0Euy, 0uy) -> op_00E0()
            | (0uy, 0uy, 0x0Euy, 0x0Euy) -> op_00EE()
            | (0uy, _, _, _) -> op_0NNN(getNNN(currentCommand))
            | (1uy, _, _, _) -> op_1NNN(getNNN(currentCommand))
            | (2uy, _, _, _) -> op_0NNN(getNNN(currentCommand))
            | (3uy, x, _, _) -> op_3XNN(x, currentCommand[1])
            | (4uy, x, _, _) -> op_4XNN(x, currentCommand[1])
            | (5uy, x, y, 0uy) -> op_5XY0(x, y)
            | (6uy, x, _, _) -> op_6XNN(x, currentCommand[1])
            | (7uy, x, _, _) -> op_7XNN(x, currentCommand[1])
            | (8uy, x, y, 0uy) -> op_8XY0(x,y)
            | (8uy, x, y, 1uy) -> op_8XY1(x,y)
            | (8uy, x, y, 2uy) -> op_8XY2(x,y)
            | (8uy, x, y, 3uy) -> op_8XY3(x,y)
            | (8uy, x, y, 4uy) -> op_8XY4(x,y)
            | (8uy, x, y, 5uy) -> op_8XY5(x,y)
            | (8uy, x, _, 6uy) -> op_8XY6(x)
            | (8uy, x, y, 7uy) -> op_8XY7(x,y)
            | (8uy, x, _, 0xEuy) -> op_8XYE(x)
            | (0x0Auy, _, _, _) -> op_ANNN(getNNN(currentCommand))
            | (0x0Buy, _, _, _) -> op_BNNN(getNNN(currentCommand))
            | (0x0Cuy, x, _, _) -> op_CXNN(x, currentCommand[1])
            | (0x0Duy, x, y, n) -> op_DXYN(x,y,n)
            | (0x0Euy, x, 9uy, 0x0Euy) -> op_EX9E(x)
            | (0x0Euy, x, 0x0Auy, 1uy) -> op_EXA1(x)
            | (0x0Fuy, x, 0uy, 7uy) -> op_FX07(x)
            | (0x0Fuy, x, 0uy, 0x0Auy) -> op_FX0A(x)
            | (0x0Fuy, x, 1uy, 5uy) -> op_FX15(x)
            | (0x0Fuy, x, 1uy, 8uy) -> op_FX18(x)
            | (0x0Fuy, x, 1uy, 0x0Euy) -> op_FX1E(x)
            | (0x0Fuy, x, 3uy, 3uy) -> op_FX33(x)
            | (0x0Fuy, x, 5uy, 5uy) -> op_FX55(x)
            | (0x0Fuy, x, 6uy, 5uy) -> op_FX65(x)
            | (_, _, _, _) -> raise (Exception("Unknown opcode"))
    with 
        | ex -> 
            let message = $"Unable to process with opcode {currentCommand[0]:X2} {currentCommand[1]:X2} at position {(programCounter-1s)*2s}"
            raise (Exception(message, ex))



