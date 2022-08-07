module Chip8Emulator

open System
open System.IO
open NLog

let logger = LogManager.GetLogger("Emulator")

let getLow4Bit octet = 
    byte(octet &&& (byte)0x0F)

let getHight4Bit octet =
    byte(octet >>> 4)

let getHigh8Bit word =
    byte(word >>> 8)

let getLow8Bit word =
    byte(word &&& 0x00FFus)
                
let getLow12Bit byte1 byte2 =
    let low0 = getLow4Bit(byte1)
    let result = BitConverter.ToUInt16([| byte2; low0 |], 0)
    result

let splitOctet octet =
    let n1 = getHight4Bit(octet)
    let n2 = getLow4Bit(octet)
    (n1, n2)

type DelayTimer () = 
    let mutable _value = 0uy

    member this.value = _value
    member this.set value= _value <- value

    member this.tick () = 
        if _value > 0uy then 
            _value <- _value - 1uy

type SoundTimer () = 
    let mutable _value = 0uy

    member this.set value= _value <- value

    member this.tick () = 
        if _value > 0uy then 
            //Console.Beep()
            _value <- _value - 1uy
                

type Input (values: byte[]) = 
    let _status = Array.create 16 false
    let _values = values

    let mutable _isWaiting = false
    let mutable _waitingNumber = 0uy


    member this.isWaiting = _isWaiting
    member this.status number = _status[number]
    member this.wait number =
        _isWaiting <- true
        _waitingNumber <- number

    member this.reset () = 
        for i in 0 .. _status.Length - 1 do
            _status[i] <- false

    member this.set number = 

        if _isWaiting then
            _isWaiting <- false
            _values[int(_waitingNumber)] <- number

        _status[int(number)] <- true

type Stack (memory: byte[], shift, length) =

    let mutable _pointer = 0

    let reverseBytes(bytes:byte[]) = [| bytes[1]; bytes[0] |]

    member this.pointer = _pointer
    member this.memory = memory.AsSpan(shift, length)  

    member this.push(value:uint16) = 

        let slice = this.memory.Slice(_pointer<<<1, 2)

        let reversed = value |> BitConverter.GetBytes |> reverseBytes
        reversed.CopyTo(slice)

        _pointer <- _pointer + 1

    member this.pop () =
        _pointer <- _pointer - 1
        let slice = this.memory.Slice(int(_pointer<<<1), 2)

        slice.ToArray() |> reverseBytes |> BitConverter.ToUInt16


type Display (width, height, memory: byte[], shift) = 

    member this.unpackedWidth = width
    member this.packedWidth = width/8
    member this.height = height
    member this.memory = memory.AsSpan(shift, this.packedWidth * height)

    member this.getUnpackedValue x y = 
        let packedX = x / 8
        let packedBit = 7 - x % 8

        let spritePosition = packedX + y* this.packedWidth

        let sprite = this.memory[spritePosition]
        let value = sprite >>> packedBit
        (value &&& 1uy) = 1uy

    member this.setPackedValue x y value = 
        let address = y * this.packedWidth + x
        this.memory[address] <- value        


type Emulator ()=

    static let _screenWidth = 64
    static let _screenHeight = 32
    static let _displayBufferLength = _screenWidth / 8 * _screenHeight
    static let _stackBufferLength = 96

    static let _totalMemory = 4096
    static let _codeSectionShift = 0x200
    static let _displaySectionShift = _totalMemory - _displayBufferLength
    static let _stackSectionShift = 0x0EA0

    let mutable _programCounter:uint16 = uint16(_codeSectionShift)
    let _memory: byte[] = Array.create _totalMemory 0uy    
    let _rnd = Random()
    let mutable _i:uint16 = 0us
    let _registers: byte[] = Array.create 16 0uy

    let _inputs = Input(_registers)
    let _display = Display(_screenWidth, _screenHeight, _memory, _displaySectionShift)
    let _stack = Stack(_memory, _stackSectionShift, _stackBufferLength)

    let _delayTimer = DelayTimer()
    let _soundTimer = SoundTimer()


    let loadCode(code: ReadOnlySpan<byte>) = 
        let targetSpan = _memory.AsSpan(int(_programCounter))
        code.CopyTo(targetSpan)

    let loadSprites() = 
        let sprites:byte[] = [|
            0xF0uy; 0x90uy; 0x90uy; 0x90uy; 0xF0uy;
            0x20uy; 0x60uy; 0x20uy; 0x20uy; 0x70uy;
            0xF0uy; 0x10uy; 0xF0uy; 0x80uy; 0xF0uy;
            0xF0uy; 0x10uy; 0xF0uy; 0x10uy; 0xF0uy;
            0x90uy; 0x90uy; 0xF0uy; 0x10uy; 0x10uy;
            0xF0uy; 0x80uy; 0xF0uy; 0x10uy; 0xF0uy;
            0xF0uy; 0x80uy; 0xF0uy; 0x90uy; 0xF0uy;
            0xF0uy; 0x10uy; 0x20uy; 0x40uy; 0x40uy;
            0xF0uy; 0x90uy; 0xF0uy; 0x90uy; 0xF0uy;
            0xF0uy; 0x90uy; 0xF0uy; 0x10uy; 0xF0uy;
            0xF0uy; 0x90uy; 0xF0uy; 0x90uy; 0x90uy;
            0xE0uy; 0x90uy; 0xE0uy; 0x90uy; 0xE0uy;
            0xF0uy; 0x80uy; 0x80uy; 0x80uy; 0xF0uy;
            0xE0uy; 0x90uy; 0x90uy; 0x90uy; 0xE0uy;
            0xF0uy; 0x80uy; 0xF0uy; 0x80uy; 0xF0uy;
            0xF0uy; 0x80uy; 0xF0uy; 0x80uy; 0x80uy
        |]

        Array.Copy(sprites, _memory, sprites.Length)
   
    let op_00E0 () = 
        for i in 0 .. (int)_displayBufferLength - 1 do
            _display.memory[i] <- 0uy
        logger.Trace "OK: Clean screen"

    let op_00EE () = 
        _programCounter <- _stack.pop()
        logger.Trace $"OK: return from subroutine to {_programCounter}"

    let op_1NNN nnn = 
        _programCounter <- nnn
        logger.Trace $"OK: jump to {_programCounter}"

    let op_2NNN nnn = 
        _stack.push(uint16(_programCounter))
        _programCounter <- nnn
        logger.Trace($"OK: Call subroutine at {_programCounter}")

    let op_3XNN x nn = 
        if _registers[(int)x] = nn then
            _programCounter <- _programCounter + 2us
        logger.Trace $"OK: Skip if v[{x}] == {nn}: {_registers[(int)x] = nn}"

    let op_4XNN x nn = 
        if _registers[(int)x] <> nn then
            _programCounter <- _programCounter + 2us
        logger.Trace $"OK: Skip if v[{x}] != {nn}: {_registers[(int)x] <> nn}"

    let op_5XY0 x y = 
        if _registers[(int)x] = _registers[(int)y] then
            _programCounter <- _programCounter + 2us
        logger.Trace $"OK: Skip if v[{x}] == v[{y}]: {_registers[(int)x] = _registers[(int)y]}"

    let op_6XNN x nn =
        _registers[(int)x] <- nn
        logger.Trace $"OK: set v[{x}] <- {nn}"

    let op_7XNN x nn =
        _registers[(int)x] <- _registers[(int)x] + nn
        logger.Trace $"OK: v[{x}] += {nn}: {_registers[(int)x]}"

    let op_8XY0 x y = 
        _registers[(int)x] <- _registers[(int)y]
        logger.Trace $"OK: v[{x}] = v[{y}]: {_registers[(int)y]}"

    let op_8XY1 x y = 
        _registers[(int)x] <- _registers[(int)x] ||| _registers[(int)y]
        logger.Trace $"OK: v[{x}] |= v[{y}]: {_registers[(int)x]}"

    let op_8XY2 x y = 
        _registers[(int)x] <- _registers[(int)x] &&& _registers[(int)y]
        logger.Trace $"OK: v[{x}] &= v[{y}]: {_registers[(int)x]}"

    let op_8XY3 x y = 
        _registers[(int)x] <- _registers[(int)x] ^^^ _registers[(int)y]
        logger.Trace $"OK: v[{x}] ^= v[{y}]: {_registers[(int)x]}"

    let op_8XY4 x y = 
        _registers[15] <- if uint16(_registers[int(x)]) + uint16(_registers[(int)y]) > uint16(Byte.MaxValue) then 1uy else 0uy
        _registers[(int)x] <- _registers[(int)x] + _registers[(int)y]
        logger.Trace $"OK: v[{x}] += v[{y}]: {_registers[(int)x]}"

    let op_8XY5 x y = 
        _registers[15] <-  if _registers[(int)y] > _registers[(int)x] then 0uy else 1uy
        _registers[(int)x] <- _registers[(int)x] - _registers[(int)y]
        logger.Trace $"OK: v[{x}] -= v[{y}]: {_registers[(int)x]}, v[0xFF]: {_registers[15]}"

    let op_8XY6 x =    
        _registers[15] <- _registers[(int)x] &&& 1uy
        _registers[(int)x] <- _registers[(int)x] >>> 1

        logger.Trace $"OK: v[{x}] = v[{x}] >> 1: {_registers[(int)x]}, v[15]={_registers[15]}"

    let op_8XY7 x y = 
        _registers[15] <- if _registers[(int)x] > _registers[(int)y] then 0uy else 1uy
        _registers[(int)x] <- _registers[(int)y] - _registers[(int)x]

        logger.Trace $"OK: v[{x}] = v[{y}] - v[{x}]: {_registers[int(x)]}, v[15]={_registers[15]}"

    let op_8XYE x = 
        _registers[15] <- (_registers[(int)x] &&& 0x80uy) >>> 7
        _registers[(int)x] <- _registers[(int)x] <<< 1

        logger.Trace $"OK: v[{x}] = v[{x}] << 1: {_registers[int(x)]}, v[15]={_registers[15]}"

    let op_9XY0 x y = 
        if _registers[int(x)] <> _registers[int(y)] then
            _programCounter <- _programCounter + 2us
        logger.Trace $"OK: Skip if v[{x}] != v[{y}]: {_registers[int(x)] <> _registers[int(y)]}"


    let op_ANNN nnn = 
        _i <- nnn

        logger.Trace $"OK: I = {nnn}"

    let op_BNNN nnn = 
        _programCounter <- uint16(_registers[0]) + nnn

        logger.Trace $"OK: pc = v[0] + {nnn}: {_programCounter}"

    let op_CXNN x nn =
        let random = (byte)(_rnd.Next(0,256))
        _registers[(int)x] <- random &&& nn

        logger.Trace $"v[{x}] = random: {_registers[(int)x]}"

    let op_DXYN x y n =

        // I'm feeling stupid to write this since there should be an ellegant solution
        let rec detectChanged (val1, val2, shift) =
            let pos1 = (val1 &&& (1uy <<< shift)) >>> shift
            let pos2 = (val2 &&& (1uy <<< shift)) >>> shift

            match (shift, pos1, pos2) with
                | (8, _, _) -> false
                | (_, 1uy, 1uy) -> true
                | (_,_,_) -> detectChanged(val1, val2, shift+1)  

        let drawSprite addr sprite = 
            let packed = _display.memory[addr]
            let xored = packed ^^^ sprite
            _display.memory[addr] <- xored
            detectChanged(packed, sprite, 0)

        let drawRow x y spriteAddress =
            let addr = x / 8 + y * _display.unpackedWidth/8
            let sprite = _memory[spriteAddress]
            let shift = x % 8

            let first = drawSprite addr (sprite>>>shift)
            let second = match (shift > 0, addr < 255) with
                            | (true, true) -> drawSprite (addr+1) (sprite<<<(8-shift))
                            | (_, _) -> false

            first || second
            

        let coordinateX = int(_registers[int(x)])
        let coordinateY = int(_registers[int(y)])

        let collisionDetected = [0..int(n)-1] 
                                    |> List.map (fun y -> (coordinateY+y, int(_i)+y))
                                    |> List.filter (fun t -> (fst t) < 32)
                                    |> List.map (fun t -> drawRow coordinateX (fst t) (snd t))
                                    |> List.fold (||) false

        _registers[15] <- if collisionDetected then 1uy else 0uy

        logger.Trace $"OK: draw screen at ({coordinateX};{coordinateY}), total {n} rows; collision {collisionDetected}"
    
    let op_EX9E x =
        let key = _inputs.status(int(_registers[int(x)]))
        if key then
            _programCounter <- _programCounter+2us

        logger.Trace $"OK: Skip if input[V[{x}]] is NOT pressed: {key}"

    let op_EXA1 x =
        let key = _inputs.status(int(_registers[int(x)]))
        if not key then
            _programCounter <- _programCounter+2us

        logger.Trace $"OK: Skip if input[V[{x}]] is pressed: {not key}"

    let op_FX07 x = 
        _registers[(int)x] <- _delayTimer.value

        logger.Trace $"OK: v[{x}] <- delayTimer"

    let op_FX0A x = 
        _inputs.wait(byte(x))
        logger.Trace $"Readkey"

    let op_FX15 x = 
        _delayTimer.set(x)

        logger.Trace $"Set delay timer to {x}"

    let op_FX18 x = 
        _soundTimer.set(x)

        logger.Trace $"Set sound timer to {x}"

    let op_FX1E x = 
        _i <- _i + (uint16)(_registers[(int)x])

        logger.Trace $"OK: I += v[{x}]: {_i}"

    let op_FX29 x =
        let number = _registers[(int)x]
        _i <- uint16(number) * 5us
        logger.Trace $"OK: I = addr({x}): {_i}"

    let op_FX33 x =
        let value = _registers[(int)x]

        _memory[(int)_i] <- value / 100uy
        _memory[(int)(_i+(uint16)1uy)] <- (value % 100uy) / 10uy
        _memory[(int)(_i+(uint16)2uy)] <- (value % 100uy) % 10uy

        logger.Trace $"OK: m[{_i}..{_i+2us} = bcd(m[{x}]): {_registers[int(x)]}"

    let op_FX55 x =
        for i in 0uy .. x do
            _memory[int(_i)+int(i)] <- _registers[(int)i]
            logger.Trace $"OK: m[{int(_i)+int(i)}] =  v[{i}]: {_memory[int(_i) + int(i)]}"

    let op_FX65 x =
        for i in 0uy .. x do
            _registers[int(i)] <- _memory[int(_i)+int(i)]
            logger.Trace $"OK: v[{i}] = m[{int(_i)+int(i)}]: {_memory[int(_i)+int(i)]}"

    let fetch () = 
        let byte1 = _memory[int(_programCounter)]
        let byte2 = _memory[int(_programCounter)+1]

        _programCounter <- _programCounter + 2us

        (byte1, byte2)

    let execute (byte1, byte2) = 
        let (firstHigh, firstLow) = splitOctet(byte1)
        let (secondHigh, secondLow) = splitOctet(byte2)

        match (firstHigh, firstLow, secondHigh, secondLow) with
                | (0uy ,0uy, 0x0Euy, 0uy) -> op_00E0()
                | (0uy, 0uy, 0x0Euy, 0x0Euy) -> op_00EE()
                | (1uy, _, _, _) -> getLow12Bit byte1 byte2 |> op_1NNN
                | (2uy, _, _, _) -> getLow12Bit byte1 byte2 |> op_2NNN
                | (3uy, x, _, _) -> op_3XNN x byte2
                | (4uy, x, _, _) -> op_4XNN x byte2
                | (5uy, x, y, 0uy) -> op_5XY0 x y
                | (6uy, x, _, _) -> op_6XNN x byte2
                | (7uy, x, _, _) -> op_7XNN x byte2
                | (8uy, x, y, 0uy) -> op_8XY0 x y
                | (8uy, x, y, 1uy) -> op_8XY1 x y
                | (8uy, x, y, 2uy) -> op_8XY2 x y
                | (8uy, x, y, 3uy) -> op_8XY3 x y
                | (8uy, x, y, 4uy) -> op_8XY4 x y
                | (8uy, x, y, 5uy) -> op_8XY5 x y
                | (8uy, x, _, 6uy) -> op_8XY6 x
                | (8uy, x, y, 7uy) -> op_8XY7 x y
                | (8uy, x, _, 0xEuy) -> op_8XYE x
                | (9uy, x, y, 0uy) -> op_9XY0 x y
                | (0x0Auy, _, _, _) -> getLow12Bit byte1 byte2 |> op_ANNN
                | (0x0Buy, _, _, _) -> getLow12Bit byte1 byte2 |> op_BNNN
                | (0x0Cuy, x, _, _) -> op_CXNN x byte2
                | (0x0Duy, x, y, n) -> op_DXYN x y n
                | (0x0Euy, x, 9uy, 0x0Euy) -> op_EX9E x
                | (0x0Euy, x, 0x0Auy, 1uy) -> op_EXA1 x
                | (0x0Fuy, x, 0uy, 7uy) -> op_FX07 x
                | (0x0Fuy, x, 0uy, 0x0Auy) -> op_FX0A x
                | (0x0Fuy, x, 1uy, 5uy) -> op_FX15 x
                | (0x0Fuy, x, 1uy, 8uy) -> op_FX18 x
                | (0x0Fuy, x, 1uy, 0x0Euy) -> op_FX1E x
                | (0xFuy, x, 2uy, 9uy) -> op_FX29 x
                | (0x0Fuy, x, 3uy, 3uy) -> op_FX33 x
                | (0x0Fuy, x, 5uy, 5uy) -> op_FX55 x
                | (0x0Fuy, x, 6uy, 5uy) -> op_FX65 x
                | (_, _, _, _) -> raise (Exception("Unknown opcode"))

    member this.displayWidth = _screenWidth
    member this.displayHeight = _screenHeight
    member this.displayMemoryShift = _displaySectionShift
    member this.codeMemoryShift = int(_codeSectionShift)
    member this.screnBufferLength = _displayBufferLength
    member this.registers = System.ReadOnlyMemory(_registers,0,_registers.Length).Span
    member this.display = _display
    member this.memory = System.ReadOnlyMemory(_memory,0,_memory.Length).Span
    member this.stack = _stack
    member this.inputs = _inputs
    member this.soundTimer = _soundTimer
    member this.delayTimer = _delayTimer
    member this.programCounter = _programCounter
    member this.i = _i

    member this.initialize(code: ReadOnlySpan<byte>) = 
        loadSprites ()
        loadCode code

    member this.setEmulatorMemory addres value = 
        _memory[addres] <- value

    member this.setEmulatorVariable number value = 
        _registers[number] <- value

    member this.setEmulatorI position = 
        _i <- position    

    member this.tick ()=

        if not this.inputs.isWaiting then
            let command1 = _memory[int(_programCounter)]
            let command2 = _memory[int(_programCounter)+1]

            try
                fetch () |> execute
            with 
                | ex -> 
                    let message = $"Unable to process with opcode {command1:X2} {command2:X2} at position {_programCounter-2us}"
                    raise (Exception(message, ex))



let loadProgramCode filename = 
    use reader = new BinaryReader(File.Open(filename, FileMode.Open, FileAccess.Read, FileShare.Read))        
    reader.ReadBytes(int(reader.BaseStream.Length)).AsSpan()

let createMemoryDump (buffer: ReadOnlySpan<byte>) = 
    let name = string(Random().NextInt64())
    use writer = new BinaryWriter(File.Create($"memory_{name}.hex"))
    writer.Write(buffer)

