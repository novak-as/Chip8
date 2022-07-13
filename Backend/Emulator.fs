module Chip8Emulator

open System
open System.IO

type nible = byte
type trible = int16 //because why not, this is just full byte + nible
type chip8Command = byte*byte

type Emulator () =

    let displayWidth = 64
    let displayHeight = 32

    let mutable _programCounter:int16 = 200s
    let _memory: byte[] = Array.create 4096 0uy
    let _rnd = Random()
    let mutable _i:int16 = 0s
    let _variables: byte[] = Array.create 16 0uy
    let _inputs:bool[] = Array.create 16 false
    let _stack:int16[] = Array.create 16 0s
    let _display: bool[,] = Array2D.create displayWidth displayHeight false
    let mutable _delayTimer:byte = 0uy
    let mutable _soundTimer:byte = 0uy
    let mutable _stackPointer:byte = 0uy

    let getLowNibble(octet):nible = 
        (byte)(octet &&& (byte)0x0F)

    let getHightNibble(octet):nible = 
        (byte)(octet >>> 4)

    let getNNN(byte1:byte, byte2:byte): trible =
        let low0 = getLowNibble(byte1)
        BitConverter.ToInt16([| byte2; low0 |], 0)

    let splitOctet(octet): nible*nible = 
        let highNibble = getHightNibble(octet)
        let lowNibble = getLowNibble(octet)

        (highNibble, lowNibble)

    let op_00EE () = 
        _stackPointer <- _stackPointer - 1uy
        let addr = _stack[(int)_stackPointer]
        _programCounter <- addr
    
    let op_00E0 () = 
        for x in 0 .. displayWidth-1 do
            for y in 0 .. displayHeight-1 do
                _display[x,y] <- false

    let op_1NNN (nnn:trible) = 
        _programCounter <- nnn 

    let op_2NNN (nnn: trible) = 
        _stack[(int)_stackPointer] <- _programCounter - 2s
        _stackPointer <- _stackPointer + 1uy
        _programCounter <- nnn
        printfn($"Call subroutine at {_programCounter}")

    let op_3XNN (x:nible, nn:byte) = 
        if _variables[(int)x] = nn then
            _programCounter <- _programCounter + 2s

    let op_4XNN (x:nible, nn:byte) = 
        if _variables[(int)x] <> nn then
            _programCounter <- _programCounter + 2s

    let op_5XY0 (x:nible, y:nible) = 
        if _variables[(int)x] = _variables[(int)y] then
            _programCounter <- _programCounter + 2s

    let op_6XNN (x:nible, nn:byte)=
        _variables[(int)x] <- nn

    let op_7XNN (x:nible, nn:byte)=
        _variables[(int)x] <- _variables[(int)x] + nn

    let op_8XY0 (x:nible, y:nible) = 
        _variables[(int)x] <- _variables[(int)y]

    let op_8XY1 (x:nible, y:nible) = 
        _variables[(int)x] <- _variables[(int)x] ||| _variables[(int)y]

    let op_8XY2 (x:nible, y:nible) = 
        _variables[(int)x] <- _variables[(int)x] &&& _variables[(int)y]

    let op_8XY3 (x:nible, y:nible) = 
        _variables[(int)x] <- _variables[(int)x] ^^^ _variables[(int)y]

    let op_8XY4 (x:nible, y:nible) = 
        _variables[(int)x] <- _variables[(int)x] + _variables[(int)y]

    let op_8XY5 (x:nible, y:nible) = 
        if _variables[(int)y] > _variables[(int)x] then 
            _variables[15] <- 0uy
        else
            _variables[15] <- 1uy

        _variables[(int)x] <- _variables[(int)x] - _variables[(int)y]

    let op_8XY6 (x:nible) =    
        _variables[15] <- _variables[(int)x] &&& 1uy
        _variables[(int)x] <- _variables[(int)x] >>> 1

    let op_8XY7 (x:nible, y:nible) = 
        if _variables[(int)x] > _variables[(int)y] then
            _variables[15] <- 0uy
        else
            _variables[15] <- 1uy

        _variables[(int)x] <- _variables[(int)y] - _variables[(int)x]

    let op_8XYE (x:nible) = 
        _variables[15] <- (_variables[(int)x] &&& 0x80uy) >>> 7
        _variables[(int)x] <- _variables[(int)x] <<< 1


    let op_ANNN (nnn:trible) = 
        _i <- nnn

    let op_BNNN (nnn:trible) = 
        _programCounter <- (int16)(_variables[0]) + nnn

    let op_CXNN (x:nible, nn:byte)=
        let random = (byte)(_rnd.Next(0,256))
        _variables[(int)x] <- random &&& nn

    let op_DXYN (x:byte, y:byte, n:byte)=

        let mutable screenUpdated = false

        for i in 0uy .. n do
            let row = _memory[(int)(_i+(int16)(i*8uy))]

            for pixel in 0uy .. 8uy do
                let bit = (row >>> (int)pixel) &&& 1uy

                if _display[(int)(x+pixel), (int)(y+i)] <> (bit = 1uy) then
                    screenUpdated <- true

                _display[(int)(x+pixel), (int)(y+i)] <- (bit = 1uy)

        if screenUpdated then
            _variables[15] <- 1uy
        else
            _variables[15] <- 0uy
    
    let op_EX9E (x:nible) =
        let key = _inputs[(int)x]
        if key then
            _programCounter <- _programCounter+2s

    let op_EXA1 (x:nible) =
        let key = _inputs[(int)x]
        if not key then
            _programCounter <- _programCounter+2s

    let op_FX07 (x:nible) = 
        _variables[(int)x] <- _delayTimer

    let op_FX0A (x:nible) = 
        _variables[(int)x] <- (byte)(Console.ReadKey().Key)

    let op_FX15 (x:nible) = 
        _delayTimer <- x

    let op_FX18 (x:nible) = 
        _soundTimer <- x

    let op_FX1E (x:nible) = 
        _i <- _i + (int16)(_variables[(int)x])

    let op_FX33 (x:nible)=
        let value = _variables[(int)x]

        _memory[(int)_i] <- value / 100uy
        _memory[(int)(_i+(int16)1uy)] <- (value % 100uy) / 10uy
        _memory[(int)(_i+(int16)2uy)] <- (value % 100uy) % 10uy

    let op_FX55 (x:nible) =
        for i in 0uy .. x do
            _memory[(int)(_i+(int16)i)] <- _variables[(int)i]

    let op_FX65 (x:nible) =
        for i in 0uy .. x do
            _variables[(int)i] <- _memory[(int)(_i+(int16)i)]

    member this.programCounter = _programCounter
    member this.command = (_memory.[(int)_programCounter], _memory.[(int)_programCounter+1])
    member this.memory = System.ReadOnlyMemory(_memory,0,_memory.Length).Span

    member this.load(filename:string)=
        let mutable index = 200
        use reader = new BinaryReader(File.Open(filename, FileMode.Open, FileAccess.Read, FileShare.Read))   

        while (reader.BaseStream.Position < reader.BaseStream.Length) do
            let bytes = reader.ReadBytes(2)
            _memory[index] <- bytes[0]
            _memory[index+1] <- bytes[1]
            index <- index + 2

    member this.tick()=
        let command1 = _memory.[(int)_programCounter]
        let command2 = _memory.[(int)_programCounter+1]


        printfn $"{_programCounter}: [0x{command1:X2} 0x{command2:X2}]"

        _programCounter <- _programCounter + 2s

        let (firstHigh, firstLow) = splitOctet(command1)
        let (secondHigh, secondLow) = splitOctet(command2)

        try
            match (firstHigh, firstLow, secondHigh, secondLow) with
                | (0uy ,0uy, 0x0Euy, 0uy) -> op_00E0()
                | (0uy, 0uy, 0x0Euy, 0x0Euy) -> op_00EE()
                | (1uy, _, _, _) -> op_1NNN(getNNN(command1, command2))
                | (2uy, _, _, _) -> op_2NNN(getNNN(command1, command2))
                | (3uy, x, _, _) -> op_3XNN(x, command2)
                | (4uy, x, _, _) -> op_4XNN(x, command2)
                | (5uy, x, y, 0uy) -> op_5XY0(x, y)
                | (6uy, x, _, _) -> op_6XNN(x, command2)
                | (7uy, x, _, _) -> op_7XNN(x, command2)
                | (8uy, x, y, 0uy) -> op_8XY0(x,y)
                | (8uy, x, y, 1uy) -> op_8XY1(x,y)
                | (8uy, x, y, 2uy) -> op_8XY2(x,y)
                | (8uy, x, y, 3uy) -> op_8XY3(x,y)
                | (8uy, x, y, 4uy) -> op_8XY4(x,y)
                | (8uy, x, y, 5uy) -> op_8XY5(x,y)
                | (8uy, x, _, 6uy) -> op_8XY6(x)
                | (8uy, x, y, 7uy) -> op_8XY7(x,y)
                | (8uy, x, _, 0xEuy) -> op_8XYE(x)
                | (0x0Auy, _, _, _) -> op_ANNN(getNNN(command1, command2))
                | (0x0Buy, _, _, _) -> op_BNNN(getNNN(command1, command2))
                | (0x0Cuy, x, _, _) -> op_CXNN(x, command2)
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
                let message = $"Unable to process with opcode {command1:X2} {command2:X2} at position {_programCounter-2s}"
                raise (Exception(message, ex))


let createMemoryDump(buffer: ReadOnlySpan<byte>) = 
    use writer = new BinaryWriter(File.Create($"{Random().NextInt64()}.hex"))
    writer.Write(buffer)