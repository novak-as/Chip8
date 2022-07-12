open System
open System.IO

let romName = "PONG"

printfn $"Loading ROM {romName}"

let loadRom(filenname) : (byte[])[] = 

    seq {
        use reader = new BinaryReader(File.Open($"C:\\Users\\onovak\\Documents\\repos_personal\\chip8\\roms\\{filenname}", FileMode.Open, FileAccess.Read, FileShare.Read))
    
        while (reader.BaseStream.Position < reader.BaseStream.Length) do
            yield reader.ReadBytes(2)

    } |> Seq.toArray

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

let getLowNibble(octet):byte = 
    (byte)(octet &&& (byte)0x0F)

let getHightNibble(octet) = 
    (byte)(octet >>> 4)

let getNNN(octets: byte[]): int16 =
    let low0 = getLowNibble(octets[0])
    BitConverter.ToInt16([| octets[1]; low0 |], 0)

let splitOctet(octet) = 
    let highNibble = getHightNibble(octet)
    let lowNibble = getLowNibble(octet)

    (highNibble, lowNibble)

let op_0NNN (octets: byte[]) = 
    let addr = getNNN(octets)/2s
    stack[(int)stackPointer] <- programCounter - 1s
    stackPointer <- stackPointer + 1uy
    programCounter <- addr

let op_00EE () = 
    stackPointer <- stackPointer - 1uy
    let addr = stack[(int)stackPointer]
    programCounter <- addr
    
let op_00E0 () = 
    for x in 0 .. display.GetLength(0) do
        for y in 0 .. display.GetLength(1) do
            display[x,y] <- false

let op_1NNN (octets: byte[]) = 
    programCounter <- getNNN(octets) / 2s

let op_3XNN (octets: byte[]) = 
    let source = getLowNibble(octets[0])
    if variables[(int)source] = octets[1] then
        programCounter <- programCounter + 1s

let op_4XNN (octets: byte[]) = 
    let source = getLowNibble(octets[0])
    if variables[(int)source] <> octets[1] then
        programCounter <- programCounter + 1s

let op_5XY0 (octets: byte[]) = 
    let source = getLowNibble(octets[0])
    let target = getHightNibble(octets[1])
    if variables[(int)source] = variables[(int)target] then
        programCounter <- programCounter + 1s

let op_6XNN (octets: byte[])=
    let source = getLowNibble(octets[0])
    variables[(int)source] <- octets[1]

let op_7XNN (octets: byte[])=
    let source = getLowNibble(octets[0])
    variables[(int)source] <- variables[(int)source] + octets[1]

let op_8XY0 (octets: byte[]) = 
    let index_to = getLowNibble(octets[0])
    let index_from = getHightNibble(octets[1])
    variables[(int)index_to] <- variables[(int)index_to]

let op_8XY1 (octets: byte[]) = 
    let source = (int)(getLowNibble(octets[0]))
    let target = (int)(getHightNibble(octets[1]))
    variables[source] <- variables[source] ||| variables[target]

let op_8XY2 (octets: byte[]) = 
    let source = (int)(getLowNibble(octets[0]))
    let target = (int)(getHightNibble(octets[1]))
    variables[source] <- variables[source] &&& variables[target]

let op_8XY3 (octets: byte[]) = 
    let source = (int)(getLowNibble(octets[0]))
    let target = (int)(getHightNibble(octets[1]))
    variables[source] <- variables[source] ^^^ variables[target]

let op_8XY4 (octets: byte[]) = 
    let source = (int)(getLowNibble(octets[0]))
    let target = (int)(getHightNibble(octets[1]))
    variables[source] <- variables[source] + variables[target]

let op_8XY5 (octets: byte[]) = 
    let x = (int)(getLowNibble(octets[0]))
    let y = (int)(getHightNibble(octets[1]))

    if variables[y] > variables[x] then 
        variables[15] <- 0uy
    else
        variables[15] <- 1uy

    variables[x] <- variables[x] - variables[y]

let op_8XY6 (octets: byte[]) = 
    let x = (int)(getLowNibble(octets[0]))
    
    variables[15] <- variables[x] &&& 1uy
    variables[x] <- variables[x] >>> 1

let op_8XY7 (octets: byte[]) = 
    let x = (int)(getLowNibble(octets[0]))
    let y = (int)(getHightNibble(octets[1]))

    if variables[x] > variables[y] then
        variables[15] <- 0uy
    else
        variables[15] <- 1uy

    variables[x] <- variables[y] - variables[x]


let op_8XYE (octets: byte[]) = 
    let x = (int)(getLowNibble(octets[0]))    
    variables[15] <- (variables[x] &&& 0x80uy) >>> 7
    variables[x] <- variables[x] <<< 1


let op_ANNN (octets: byte[]) = 
    I <- getNNN(octets)

let op_BNNN (octets: byte[]) = 
    programCounter <- (int16)(variables[0]) + getNNN(octets)

let op_CXNN (octets:byte[])=
    let random = (byte)(rnd.Next(0,256))
    let source = (int)(getLowNibble(octets[0]))
    variables[source] <- random &&& octets[1]

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
    


let op_EX9E (octets:byte[]) =
    let source = (int)(getLowNibble(octets[0]))
    let key = inputs[source]
    if key then
        programCounter <- programCounter+1s

let op_EXA1 (octets:byte[]) =
    let source = (int)(getLowNibble(octets[0]))
    let key = inputs[source]
    if not key then
        programCounter <- programCounter+1s

let op_FX07 (octets: byte[]) = 
    let x = (int)(getLowNibble(octets[0]))
    variables[x] <- delayTimer

let op_FX0A (octets:byte[]) = 
    let x = (int)(getLowNibble(octets[0]))
    variables[x] <- (byte)(Console.ReadKey().Key)

let op_FX15 (x:byte) = 
    delayTimer <- x

let op_FX18 (x:byte) = 
    soundTimer <- x

let op_FX1E (x:byte) = 
    I <- I + (int16)(variables[(int)x])

let op_FX33 (x:byte)=
    let value = variables[(int)x]

    memory[(int)I] <- value / 100uy
    memory[(int)(I+(int16)1uy)] <- (value % 100uy) / 10uy
    memory[(int)(I+(int16)2uy)] <- (value % 100uy) % 10uy

let op_FX55 (x:byte) =
    for i in 0uy .. x do
        memory[(int)(I+(int16)i)] <- variables[(int)i]

let op_FX65 (x:byte) =
    for i in 0uy .. x do
        variables[(int)i] <- memory[(int)(I+(int16)i)]

while true do

    let currentCommand = data.[(int)programCounter]

    printfn $"{programCounter}: [0x{currentCommand[0]:X2} 0x{currentCommand[1]:X2}]"

    programCounter <- programCounter + 1s

    let (firstHigh, firstLow) = splitOctet(currentCommand[0])
    let (secondHigh, secondLow) = splitOctet(currentCommand[1])

    try
        match (firstHigh, firstLow, secondHigh, secondLow) with
            | (0uy ,0uy, 0x0Euy, 0uy) -> op_00E0()
            | (0uy, 0uy, 0x0Euy, 0x0Euy) -> op_00EE()
            | (0uy, _, _, _) -> op_0NNN(currentCommand)
            | (1uy, _, _, _) -> op_1NNN(currentCommand)
            | (2uy, _, _, _) -> op_0NNN(currentCommand)
            | (3uy, _, _, _) -> op_3XNN(currentCommand)
            | (4uy, _, _, _) -> op_4XNN(currentCommand)
            | (5uy, _, _, _) -> op_5XY0(currentCommand)
            | (6uy, _, _, _) -> op_6XNN(currentCommand)
            | (7uy, _, _, _) -> op_7XNN(currentCommand)
            | (8uy, _, _, 0uy) -> op_8XY0(currentCommand)
            | (8uy, _, _, 1uy) -> op_8XY1(currentCommand)
            | (8uy, _, _, 2uy) -> op_8XY2(currentCommand)
            | (8uy, _, _, 3uy) -> op_8XY3(currentCommand)
            | (8uy, _, _, 4uy) -> op_8XY4(currentCommand)
            | (8uy, _, _, 5uy) -> op_8XY5(currentCommand)
            | (8uy, _, _, 6uy) -> op_8XY6(currentCommand)
            | (8uy, _, _, 7uy) -> op_8XY7(currentCommand)
            | (8uy, _, _, 0xEuy) -> op_8XYE(currentCommand)
            | (0x0Auy, _, _, _) -> op_ANNN(currentCommand)
            | (0x0Buy, _, _, _) -> op_BNNN(currentCommand)
            | (0x0Cuy, _, _, _) -> op_CXNN(currentCommand)
            | (0x0Duy, x, y, n) -> op_DXYN(x,y,n)
            | (0x0Euy, _, 9uy, 0x0Euy) -> op_EX9E(currentCommand)
            | (0x0Euy, _, 0x0Auy, 1uy) -> op_EXA1(currentCommand)
            | (0x0Fuy, _, 0uy, 7uy) -> op_FX07(currentCommand)
            | (0x0Fuy, _, 0uy, 0x0Auy) -> op_FX0A(currentCommand)
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
            Console.WriteLine message
            raise (Exception(message, ex))



