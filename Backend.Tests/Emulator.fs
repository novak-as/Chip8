﻿namespace Backend.Tests


open System
open NUnit.Framework
open Chip8Emulator


[<TestFixture>]
type ToolsTests () =

    [<Test>]
    member this.getLow4Bit_correct() =
        let nible = Chip8Emulator.getLow4Bit(0xF1uy)
        Assert.AreEqual(0x01uy, nible)

    [<Test>]
    member this.getHigh4Bit_correct() =
        let nible = Chip8Emulator.getHight4Bit(0xF1uy)
        Assert.AreEqual(0x0Fuy, nible)

    [<Test>]
    member this.getLow12Bit_correct() =
        let bit12 = Chip8Emulator.getLow12Bit 0xABuy 0xCDuy
        Assert.AreEqual(0x0BCDus, bit12)

    [<Test>]
    member this.splitOctet_correct() =
        let (nible1, nible2) = Chip8Emulator.splitOctet(0xABuy)
        Assert.AreEqual(0x0Auy, nible1)
        Assert.AreEqual(0x0Buy, nible2)

[<TestFixture>]
type StackTests () = 
    [<Test>]
    member this.push_correct() = 
        let buffer = Array.create 10 0uy
        let stack = Stack(buffer,5,5)

        stack.push(0x0F0Aus)
        
        Assert.AreEqual(1, stack.pointer)
        Assert.AreEqual(0x0Fuy, buffer[5])
        Assert.AreEqual(0x0Auy, buffer[6])

    [<Test>]
    member this.pop_correct() = 
        let buffer = Array.create 10 0uy
        let stack = Stack(buffer,5,5)

        stack.push(0x0F0Aus)

        let value = stack.pop()
        
        Assert.AreEqual(0, stack.pointer)
        Assert.AreEqual(0x0F0Aus, value)

type DisplayTests () = 

    [<TestCase(0,0)>]
    [<TestCase(7,0)>]
    [<TestCase(0,31)>]
    [<TestCase(7,31)>]
    member this.setDisplayPackedByte2D_correct(x,y) = 
        let buffer = Array.create 256 0uy
        let display = Display(64, 32, buffer, 0)

        display.setPackedValue x y 0xAAuy

        let addr = x + display.unpackedWidth/8 * y
        Assert.AreEqual(0xAAuy, display.memory[addr])

    [<TestCase(0,0)>]
    [<TestCase(7,0)>]
    [<TestCase(0,31)>]
    [<TestCase(7,31)>]
    member this.getDisplayUnpackedByte_correct(x,y) = 
        let buffer = Array.create 256 0uy
        let display = Display(64, 32, buffer, 0)

        display.memory[x + display.unpackedWidth/8*y] <- 0xA9uy

        Assert.AreEqual(true, display.getUnpackedValue (x*8) y)
        Assert.AreEqual(false, display.getUnpackedValue (x*8+1) y)
        Assert.AreEqual(true, display.getUnpackedValue (x*8+2) y)
        Assert.AreEqual(false, display.getUnpackedValue (x*8+3) y)
        Assert.AreEqual(true, display.getUnpackedValue (x*8+4) y)
        Assert.AreEqual(false, display.getUnpackedValue (x*8+5) y)
        Assert.AreEqual(false, display.getUnpackedValue (x*8+6) y)
        Assert.AreEqual(true, display.getUnpackedValue (x*8+7) y)

[<TestFixture>]
type EmulatorTests ()=
    [<Test>]
    member this.initialize_correct() = 
        let emulator = Emulator()

        let code = [| 0xABuy; 0xCDuy |].AsSpan()

        emulator.initialize(code)

        Assert.AreEqual(0xABuy, emulator.memory[emulator.codeMemoryShift])
        Assert.AreEqual(0xCDuy, emulator.memory[emulator.codeMemoryShift+1])

    [<Test>]
    member this.setVMMemory_correct() = 
        let emulator = Emulator()

        emulator.setEmulatorMemory emulator.displayMemoryShift 0xFFuy
        Assert.AreEqual(0xFFuy, emulator.memory[emulator.displayMemoryShift])

    [<Test>]
    member this.op00E0_correct() = 
        let emulator = Emulator()

        emulator.setEmulatorMemory emulator.displayMemoryShift 0xFFuy
        emulator.initialize([| 0x00uy; 0xE0uy |].AsSpan())
        emulator.tick()

        Assert.AreEqual(0x00uy, emulator.display.memory[0])

    [<Test>]
    member this.op1NNN_correct() = 
        let emulator = Emulator()

        emulator.initialize([| 0x16uy; 0x66uy |].AsSpan())
        emulator.tick()

        Assert.AreEqual(0x0666us, emulator.programCounter)

    [<Test>]
    member this.op00EE_correct() = 
        let emulator = Emulator()

        let code = [| 0x26uy; 0x66uy; |].AsSpan()

        emulator.initialize(code)

        //add return at correct address
        emulator.setEmulatorMemory 0x0666 0x00uy
        emulator.setEmulatorMemory 0x0667 0xEEuy

        let pc = emulator.programCounter
        emulator.tick()
        emulator.tick()

        Assert.AreEqual(pc+2us, emulator.programCounter)

    [<Test>]
    member this.op2NNN_correct() = 
        let emulator = Emulator()

        let code = [| 0x26uy; 0x66uy |].AsSpan()

        emulator.initialize(code)
        emulator.tick()

        Assert.AreEqual(0x0666us, emulator.programCounter)
        Assert.AreEqual(1,emulator.stack.pointer)
        Assert.AreEqual(0x02uy, emulator.stack.memory[0])
        Assert.AreEqual(0x02uy, emulator.stack.memory[1])

    [<TestCase(0xFFuy, 4us)>]
    [<TestCase(0xF5uy, 2us)>]
    member this.op3XNN_correct(variable, shift) = 
        let emulator = Emulator()

        let code = [| 0x30uy; 0xFFuy; |].AsSpan()

        let pc = emulator.programCounter
        emulator.initialize(code)
        emulator.setEmulatorVariable 0 variable
        emulator.tick()

        Assert.AreEqual(pc+shift, emulator.programCounter)

    [<TestCase(0xFFuy, 2us)>]
    [<TestCase(0xF5uy, 4us)>]
    member this.op4XNN_correct(variable, shift) = 
        let emulator = Emulator()

        let code = [| 0x40uy; 0xFFuy; |].AsSpan()

        let pc = emulator.programCounter
        emulator.initialize(code)
        emulator.setEmulatorVariable 0 variable
        emulator.tick()

        Assert.AreEqual(pc+shift, emulator.programCounter)

    [<TestCase(0xFFuy, 0xFFuy, 4us)>]
    [<TestCase(0xFFuy, 0xF5uy, 2us)>]
    member this.op5XY0_correct(variable1, variable2, shift) = 
        let emulator = Emulator()

        let code = [| 0x50uy; 0x10uy; |].AsSpan()

        let pc = emulator.programCounter
        emulator.initialize(code)
        emulator.setEmulatorVariable 0 variable1
        emulator.setEmulatorVariable 1 variable2
        emulator.tick()

        Assert.AreEqual(pc+shift, emulator.programCounter)

    [<Test>]
    member this.op6XNN_correct() = 
        let emulator = Emulator()

        let code = [| 0x60uy; 0xFFuy |].AsSpan()

        emulator.initialize(code)
        emulator.tick()

        Assert.AreEqual(0xFFuy, emulator.registers[0])

    [<Test>]
    member this.op7XNN_correct() = 
        let emulator = Emulator()

        let code = [| 0x70uy; 0x01uy |].AsSpan()

        emulator.initialize(code)
        emulator.setEmulatorVariable 0 1uy
        emulator.tick()

        Assert.AreEqual(0x02uy, emulator.registers[0])

    [<Test>]
    member this.op8XY0_correct() = 
        let emulator = Emulator()

        let code = [| 0x80uy; 0x10uy |].AsSpan()

        emulator.initialize(code)
        emulator.setEmulatorVariable 0 1uy
        emulator.setEmulatorVariable 1 2uy
        emulator.tick()

        Assert.AreEqual(2uy, emulator.registers[0])

    [<Test>]
    member this.op8XY1_correct() = 
        let emulator = Emulator()

        let code = [| 0x80uy; 0x11uy |].AsSpan()

        emulator.initialize(code)
        emulator.setEmulatorVariable 0 0xF0uy
        emulator.setEmulatorVariable 1 0x0Fuy
        emulator.tick()

        Assert.AreEqual(0xFFuy, emulator.registers[0])

    [<Test>]
    member this.op8XY2_correct() = 
        let emulator = Emulator()

        let code = [| 0x80uy; 0x12uy |].AsSpan()

        emulator.initialize(code)
        emulator.setEmulatorVariable 0 0xF1uy
        emulator.setEmulatorVariable 1 0x1Fuy
        emulator.tick()

        Assert.AreEqual(0x11uy, emulator.registers[0])

    [<Test>]
    member this.op8XY3_correct() = 
        let emulator = Emulator()

        let code = [| 0x80uy; 0x13uy |].AsSpan()

        emulator.initialize(code)
        emulator.setEmulatorVariable 0 0xF1uy
        emulator.setEmulatorVariable 1 0x1Fuy
        emulator.tick()

        Assert.AreEqual(0xEEuy, emulator.registers[0])

    [<TestCase(1uy, 2uy, 3uy, 0uy)>]
    [<TestCase(1uy, 255uy, 0uy, 1uy)>]
    member this.op8XY4_correct(val1, val2, result, carryover) = 
        let emulator = Emulator()

        let code = [| 0x80uy; 0x14uy |].AsSpan()

        emulator.initialize(code)
        emulator.setEmulatorVariable 0 val1
        emulator.setEmulatorVariable 1 val2
        emulator.tick()

        Assert.AreEqual(result, emulator.registers[0])
        Assert.AreEqual(carryover, emulator.registers[15])

    [<TestCase(1uy, 2uy, 255uy, 0uy)>]
    [<TestCase(2uy, 1uy, 1uy, 1uy)>]
    member this.op8XY5_correct(val1, val2, result, carryover) = 
        let emulator = Emulator()

        let code = [| 0x80uy; 0x15uy |].AsSpan()

        emulator.initialize(code)
        emulator.setEmulatorVariable 0 val1
        emulator.setEmulatorVariable 1 val2
        emulator.tick()

        Assert.AreEqual(result, emulator.registers[0])
        Assert.AreEqual(carryover, emulator.registers[15])

    [<TestCase(0xFFuy, 0x7Fuy, 1us)>]
    [<TestCase(0xFEuy, 0x7Fuy, 0us)>]
    member this.op8XY6_correct(initial, afterShift, carryover) = 
        let emulator = Emulator()

        let code = [| 0x80uy; 0x16uy |].AsSpan()

        emulator.initialize(code)
        emulator.setEmulatorVariable 0 initial
        emulator.tick()

        Assert.AreEqual(afterShift, emulator.registers[0])
        Assert.AreEqual(carryover, emulator.registers[15])

    [<TestCase(5uy, 2uy, 253uy, 0us)>]
    [<TestCase(5uy, 7uy, 2uy, 1us)>]
    member this.op8XY7_correct(val1, val2, result, carryover) = 
        let emulator = Emulator()

        let code = [| 0x80uy; 0x17uy |].AsSpan()

        emulator.initialize(code)
        emulator.setEmulatorVariable 0 val1
        emulator.setEmulatorVariable 1 val2
        emulator.tick()

        Assert.AreEqual(result, emulator.registers[0])
        Assert.AreEqual(carryover, emulator.registers[15])

    [<TestCase(0xFFuy, 0xFEuy, 1us)>]
    [<TestCase(0x7Fuy, 0xFEuy, 0us)>]
    member this.op8XYE_correct(initial, afterShift, carryover) = 
        let emulator = Emulator()

        let code = [| 0x80uy; 0x1Euy |].AsSpan()

        emulator.initialize(code)
        emulator.setEmulatorVariable 0 initial
        emulator.tick()

        Assert.AreEqual(afterShift, emulator.registers[0])
        Assert.AreEqual(carryover, emulator.registers[15])

    [<TestCase(1uy, 1uy, 2uy)>]
    [<TestCase(2uy, 1uy, 4uy)>]
    member this.op9XY0_correct(val1, val2, shift) = 
        let emulator = Emulator()

        let code = [| 0x90uy; 0x10uy; |].AsSpan()

        let pc = emulator.programCounter
        emulator.initialize(code)
        emulator.setEmulatorVariable 0 val1
        emulator.setEmulatorVariable 1 val2
        emulator.tick()

        Assert.AreEqual(pc+shift, emulator.programCounter)

    [<Test>]
    member this.opANNN_correct() = 
        let emulator = Emulator()

        let code = [| 0xAFuy; 0xFFuy; |].AsSpan()

        emulator.initialize(code)
        emulator.tick()

        Assert.AreEqual(0x0FFFus, emulator.i)

    [<Test>]
    member this.opBNNN_correct() = 
        let emulator = Emulator()

        let code = [| 0xB1uy; 0x24uy; |].AsSpan()

        emulator.initialize(code)
        emulator.setEmulatorVariable 0 2uy
        emulator.tick()

        Assert.AreEqual(294, emulator.programCounter)

    [<TestCase(0uy, 1uy)>]
    [<TestCase(0uy, 255uy)>]
    [<Repeat(10)>]
    member this.opCXNN_correct(min, max) = 
        let emulator = Emulator()

        let code = [| 0xC0uy; max; |].AsSpan()

        emulator.initialize(code)
        emulator.tick()

        Assert.That(emulator.registers[0]>=min && emulator.registers[0]<=max)

    [<TestCase(0uy, 0uy, 0b11001011uy, 0, 0b11001011uy, 1, 0uy)>] // top left corner
    [<TestCase(56uy, 0uy, 0b11001011uy, 7, 0b11001011uy, 8, 0uy)>] // top right corner
    [<TestCase(0uy, 31uy, 0b11001011uy, 248, 0b11001011uy, 249, 0uy)>] // bottom left corner
    [<TestCase(56uy, 31uy, 0b11001011uy, 254, 0uy, 255, 0b11001011uy)>] // bottom right corner
    [<TestCase(1uy, 1uy, 0b11001011uy, 8, 0b01100101uy, 9, 0b10000000uy)>] // affect multiple packed bytes
    [<TestCase(56uy, 0uy, 0b11001011uy, 7, 0b11001011uy, 8, 0uy)>] //can draw last row byte
    [<TestCase(56uy, 31uy, 0b11001011uy, 254, 0uy, 255, 0b11001011uy)>] //can draw the last byte
    [<TestCase(63uy, 31uy, 0b11001011uy, 254, 0uy, 255, 1uy)>] //don't try to shift beyond display memory
    member this.opDXYN_1rowSprite_correct(x,y,sprite, displayPosition1, result1, displayPosition2, result2) = 
        let emulator = Emulator()

        let code = [| 0xD0uy; 0x11uy|].AsSpan()

        emulator.initialize(code)

        //sprite to draw
        emulator.setEmulatorMemory 0 sprite
        emulator.setEmulatorI(0us)

        //position to draw
        emulator.setEmulatorVariable 0 x
        emulator.setEmulatorVariable 1 y

        //draw
        emulator.tick()

        Assert.AreEqual(result1, emulator.display.memory[displayPosition1])
        Assert.AreEqual(result2, emulator.display.memory[displayPosition2])


    [<TestCase(0uy, 0uy, 0xA7uy, 0x7Auy, 0, 0xA7uy, 1, 0uy, 8, 0x7Auy, 9, 0uy)>] // top left
    [<TestCase(56uy, 0uy, 0xA7uy, 0x7Auy, 7, 0xA7uy, 8, 0uy, 15, 0x7Auy, 16, 0uy)>] // top right
    [<TestCase(0uy, 30uy, 0xA7uy, 0x7Auy, 240, 0xA7uy, 241, 0uy, 248, 0x7Auy, 249, 0uy)>] // bottom left corner
    [<TestCase(56uy, 30uy, 0xA7uy, 0x7Auy, 247, 0xA7uy, 248, 0uy, 255, 0x7Auy, 255, 0x7Auy)>] // bottom right corner
    member this.opDXYN_2RowSprite_correct(x,y, sprite1, sprite2,
            displayPosition1, result1,  displayPosition2, result2,
            displayPosition3, result3, displayPosition4, result4) = 
        let emulator = Emulator()

        let code = [| 0xD0uy; 0x12uy|].AsSpan()

        emulator.initialize(code)

        //sprite to draw
        emulator.setEmulatorMemory 0 sprite1
        emulator.setEmulatorMemory 1 sprite2
        emulator.setEmulatorI(0us)

        //position to draw
        emulator.setEmulatorVariable 0 x
        emulator.setEmulatorVariable 1 y

        //draw
        emulator.tick()

        Assert.AreEqual(result1, emulator.display.memory[displayPosition1])
        Assert.AreEqual(result2, emulator.display.memory[displayPosition2])
        Assert.AreEqual(result3, emulator.display.memory[displayPosition3])
        Assert.AreEqual(result4, emulator.display.memory[displayPosition4])

    [<TestCase(0b11000000uy, 2, 0uy, 0uy, 0uy)>]
    [<TestCase(0b11000000uy, 2, 0xFFuy, 0uy, 1uy)>]
    [<TestCase(0b00111111uy, 2, 0uy, 0b00111111uy, 0uy)>]
    [<TestCase(0b00111111uy, 2, 0uy, 0b10111111uy, 1uy)>]
    member this.opDXYN_R15_correct(sprite, x, oldDisplay1, oldDisplay2, result) = 
        let emulator = Emulator()

        let code = [| 0xD0uy; 0x11uy|].AsSpan()
        emulator.initialize(code)

        //sprite to draw
        emulator.setEmulatorMemory 0 sprite
        emulator.setEmulatorI(0us)

        //position to draw
        emulator.setEmulatorVariable 0 x
        emulator.setEmulatorVariable 1 0uy

        //display state
        emulator.setEmulatorMemory emulator.displayMemoryShift oldDisplay1
        emulator.setEmulatorMemory (emulator.displayMemoryShift+1) oldDisplay2

        //draw
        emulator.tick()

        Assert.AreEqual(result, emulator.registers[15])

    [<Test>]
    member this.opEX9E_correct() = 
        let emulator = Emulator()

        let code = [| 0xE0uy; 0x9Euy |].AsSpan()

        emulator.initialize(code)
        let ps = emulator.programCounter

        emulator.inputs.reset ()
        emulator.inputs.set 0uy
        emulator.setEmulatorVariable 0 0uy
        emulator.tick ()

        Assert.AreEqual(ps+4us, emulator.programCounter)

    [<Test>]
    member this.opEXA1_correct() = 
        let emulator = Emulator()

        let code = [| 0xE0uy; 0xA1uy |].AsSpan()

        emulator.initialize(code)
        let ps = emulator.programCounter

        emulator.inputs.reset() //no key pressed
        emulator.setEmulatorVariable 0 0uy
        emulator.tick()

        Assert.AreEqual(ps+4us, emulator.programCounter)

    [<Test>]
    member this.opFX1E_correct() = 
        let emulator = Emulator()

        let code = [| 0xF0uy; 0x1Euy |].AsSpan()

        emulator.initialize(code)
        emulator.setEmulatorI(10us)
        emulator.setEmulatorVariable 0 1uy
        emulator.tick()

        Assert.AreEqual(11, emulator.i)
        
    [<Test>]
    member this.opFX29_correct() = 
        let emulator = Emulator()

        let code = [| 0xF0uy; 0x29uy |].AsSpan()

        emulator.initialize(code)
        emulator.setEmulatorVariable 0 5uy
        emulator.tick()

        Assert.AreEqual(25us, emulator.i)

    [<TestCase(1uy, 0uy, 0uy, 1uy)>]
    [<TestCase(12uy, 0uy, 1uy, 2uy)>]
    [<TestCase(123uy, 1uy, 2uy, 3uy)>]
    member this.opFX33_correct(input:byte, result1:byte, result2:byte, result3:byte)=
        let emulator = Emulator()

        let code = [| 0xF0uy; 0x33uy |].AsSpan()
        emulator.initialize(code)
        emulator.setEmulatorVariable 0 input
        emulator.setEmulatorMemory 0 0uy
        emulator.setEmulatorMemory 1 0uy
        emulator.setEmulatorMemory 2 0uy
        emulator.setEmulatorI(0us)
        emulator.tick()

        Assert.AreEqual(result1, emulator.memory[0])
        Assert.AreEqual(result2, emulator.memory[1])
        Assert.AreEqual(result3, emulator.memory[2])

    [<Test>]
    member this.opFX55_correct() = 
        let emulator = Emulator()

        let code = [| 0xFFuy; 0x55uy |].AsSpan()

        emulator.initialize(code)
        emulator.setEmulatorI(1us)
        for i in 0 .. 15 do
            emulator.setEmulatorVariable i 10uy

        emulator.tick()

        for i in 0 .. 15 do
            Assert.AreEqual(10us, emulator.memory[1+i])
        Assert.AreEqual(1, emulator.i)

    [<Test>]
    member this.opFX65_correct() = 
        let emulator = Emulator()

        let code = [| 0xFFuy; 0x65uy |].AsSpan()

        emulator.initialize(code)
        emulator.setEmulatorI(1us)
        for i in 0 .. 15 do
            emulator.setEmulatorMemory (1+i) 10uy

        emulator.tick()

        for i in 0 .. 15 do
            Assert.AreEqual(10us, emulator.registers[i])

        Assert.AreEqual(1, emulator.i)