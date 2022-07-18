namespace Backend.Tests


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
        let bit12 = Chip8Emulator.getLow12Bit(0xABuy, 0xCDuy)
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

        display.setDisplayPackedByte2D(x,y, 0xAAuy)

        let addr = x + display.width/8 * y
        Assert.AreEqual(0xAAuy, display.memory[addr])

    [<TestCase(0,0)>]
    [<TestCase(7,0)>]
    [<TestCase(0,31)>]
    [<TestCase(7,31)>]
    member this.getDisplayUnpackedByte_correct(x,y) = 
        let buffer = Array.create 256 0uy
        let display = Display(64, 32, buffer, 0)

        display.memory[x + display.width/8*y] <- 0xA9uy

        Assert.AreEqual(true, display.getDisplayUnpackedByte(x*8,y))
        Assert.AreEqual(false, display.getDisplayUnpackedByte(x*8+1,y))
        Assert.AreEqual(true, display.getDisplayUnpackedByte(x*8+2,y))
        Assert.AreEqual(false, display.getDisplayUnpackedByte(x*8+3,y))
        Assert.AreEqual(true, display.getDisplayUnpackedByte(x*8+4,y))
        Assert.AreEqual(false, display.getDisplayUnpackedByte(x*8+5,y))
        Assert.AreEqual(false, display.getDisplayUnpackedByte(x*8+6,y))
        Assert.AreEqual(true, display.getDisplayUnpackedByte(x*8+7,y))

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

        emulator.setVMMemory(emulator.displayMemoryShift, 0xFFuy)
        Assert.AreEqual(0xFFuy, emulator.memory[emulator.displayMemoryShift])

    [<Test>]
    member this.op00E0_correct() = 
        let emulator = Emulator()

        emulator.setVMMemory(emulator.displayMemoryShift, 0xFFuy)
        emulator.initialize([| 0x00uy; 0xE0uy |].AsSpan())
        emulator.tick()

        Assert.AreEqual(0x00uy, emulator.display[0])

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
        emulator.setVMMemory(0x0666,  0x00uy)
        emulator.setVMMemory(0x0667,  0xEEuy)

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
        Assert.AreEqual(1,emulator.stackPointer)
        Assert.AreEqual(0x02uy, emulator.stack[0])
        Assert.AreEqual(0x02uy, emulator.stack[1])

    [<TestCase(0xFFuy, 4us)>]
    [<TestCase(0xF5uy, 2us)>]
    member this.op3XNN_correct(variable, shift) = 
        let emulator = Emulator()

        let code = [| 0x30uy; 0xFFuy; |].AsSpan()

        let pc = emulator.programCounter
        emulator.initialize(code)
        emulator.setVMVariable(0, variable)
        emulator.tick()

        Assert.AreEqual(pc+shift, emulator.programCounter)

    [<TestCase(0xFFuy, 2us)>]
    [<TestCase(0xF5uy, 4us)>]
    member this.op4XNN_correct(variable, shift) = 
        let emulator = Emulator()

        let code = [| 0x40uy; 0xFFuy; |].AsSpan()

        let pc = emulator.programCounter
        emulator.initialize(code)
        emulator.setVMVariable(0, variable)
        emulator.tick()

        Assert.AreEqual(pc+shift, emulator.programCounter)

    [<TestCase(0xFFuy, 0xFFuy, 4us)>]
    [<TestCase(0xFFuy, 0xF5uy, 2us)>]
    member this.op5XY0_correct(variable1, variable2, shift) = 
        let emulator = Emulator()

        let code = [| 0x50uy; 0x10uy; |].AsSpan()

        let pc = emulator.programCounter
        emulator.initialize(code)
        emulator.setVMVariable(0, variable1)
        emulator.setVMVariable(1, variable2)
        emulator.tick()

        Assert.AreEqual(pc+shift, emulator.programCounter)

    [<Test>]
    member this.op6XNN_correct() = 
        let emulator = Emulator()

        let code = [| 0x60uy; 0xFFuy |].AsSpan()

        emulator.initialize(code)
        emulator.tick()

        Assert.AreEqual(0xFFuy, emulator.variables[0])

    [<Test>]
    member this.op7XNN_correct() = 
        let emulator = Emulator()

        let code = [| 0x70uy; 0x01uy |].AsSpan()

        emulator.initialize(code)
        emulator.setVMVariable(0, 1uy)
        emulator.tick()

        Assert.AreEqual(0x02uy, emulator.variables[0])

    [<Test>]
    member this.op8XY0_correct() = 
        let emulator = Emulator()

        let code = [| 0x80uy; 0x10uy |].AsSpan()

        emulator.initialize(code)
        emulator.setVMVariable(0, 1uy)
        emulator.setVMVariable(1, 2uy)
        emulator.tick()

        Assert.AreEqual(2uy, emulator.variables[0])

    [<Test>]
    member this.op8XY1_correct() = 
        let emulator = Emulator()

        let code = [| 0x80uy; 0x11uy |].AsSpan()

        emulator.initialize(code)
        emulator.setVMVariable(0, 0xF0uy)
        emulator.setVMVariable(1, 0x0Fuy)
        emulator.tick()

        Assert.AreEqual(0xFFuy, emulator.variables[0])

    [<Test>]
    member this.op8XY2_correct() = 
        let emulator = Emulator()

        let code = [| 0x80uy; 0x12uy |].AsSpan()

        emulator.initialize(code)
        emulator.setVMVariable(0, 0xF1uy)
        emulator.setVMVariable(1, 0x1Fuy)
        emulator.tick()

        Assert.AreEqual(0x11uy, emulator.variables[0])

    [<Test>]
    member this.op8XY3_correct() = 
        let emulator = Emulator()

        let code = [| 0x80uy; 0x13uy |].AsSpan()

        emulator.initialize(code)
        emulator.setVMVariable(0, 0xF1uy)
        emulator.setVMVariable(1, 0x1Fuy)
        emulator.tick()

        Assert.AreEqual(0xEEuy, emulator.variables[0])

    [<TestCase(1uy, 2uy, 3uy, 0uy)>]
    [<TestCase(1uy, 255uy, 0uy, 1uy)>]
    member this.op8XY4_correct(val1, val2, result, carryover) = 
        let emulator = Emulator()

        let code = [| 0x80uy; 0x14uy |].AsSpan()

        emulator.initialize(code)
        emulator.setVMVariable(0, val1)
        emulator.setVMVariable(1, val2)
        emulator.tick()

        Assert.AreEqual(result, emulator.variables[0])
        Assert.AreEqual(carryover, emulator.variables[15])

    [<TestCase(1uy, 2uy, 255uy, 0uy)>]
    [<TestCase(2uy, 1uy, 1uy, 1uy)>]
    member this.op8XY5_correct(val1, val2, result, carryover) = 
        let emulator = Emulator()

        let code = [| 0x80uy; 0x15uy |].AsSpan()

        emulator.initialize(code)
        emulator.setVMVariable(0, val1)
        emulator.setVMVariable(1, val2)
        emulator.tick()

        Assert.AreEqual(result, emulator.variables[0])
        Assert.AreEqual(carryover, emulator.variables[15])

    [<TestCase(0xFFuy, 0x7Fuy, 1us)>]
    [<TestCase(0xFEuy, 0x7Fuy, 0us)>]
    member this.op8XY6_correct(initial, afterShift, carryover) = 
        let emulator = Emulator()

        let code = [| 0x80uy; 0x16uy |].AsSpan()

        emulator.initialize(code)
        emulator.setVMVariable(0, initial)
        emulator.tick()

        Assert.AreEqual(afterShift, emulator.variables[0])
        Assert.AreEqual(carryover, emulator.variables[15])

    [<TestCase(5uy, 2uy, 253uy, 0us)>]
    [<TestCase(5uy, 7uy, 2uy, 1us)>]
    member this.op8XY7_correct(val1, val2, result, carryover) = 
        let emulator = Emulator()

        let code = [| 0x80uy; 0x17uy |].AsSpan()

        emulator.initialize(code)
        emulator.setVMVariable(0, val1)
        emulator.setVMVariable(1, val2)
        emulator.tick()

        Assert.AreEqual(result, emulator.variables[0])
        Assert.AreEqual(carryover, emulator.variables[15])

    [<TestCase(0xFFuy, 0xFEuy, 1us)>]
    [<TestCase(0x7Fuy, 0xFEuy, 0us)>]
    member this.op8XYE_correct(initial, afterShift, carryover) = 
        let emulator = Emulator()

        let code = [| 0x80uy; 0x1Euy |].AsSpan()

        emulator.initialize(code)
        emulator.setVMVariable(0, initial)
        emulator.tick()

        Assert.AreEqual(afterShift, emulator.variables[0])
        Assert.AreEqual(carryover, emulator.variables[15])

    [<TestCase(1uy, 1uy, 2uy)>]
    [<TestCase(2uy, 1uy, 4uy)>]
    member this.op9XY0_correct(val1, val2, shift) = 
        let emulator = Emulator()

        let code = [| 0x90uy; 0x10uy; |].AsSpan()

        let pc = emulator.programCounter
        emulator.initialize(code)
        emulator.setVMVariable(0, val1)
        emulator.setVMVariable(1, val2)
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
        emulator.setVMVariable(0, 2uy)
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

        Assert.That(emulator.variables[0]>=min && emulator.variables[0]<=max)

    [<TestCase(0uy, 0uy, 0xA7uy, 0, 0xA7uy, 1, 0uy)>]
    [<TestCase(1uy, 0uy, 0xA7uy, 0, 0x53uy, 1, 0x80uy)>]
    [<TestCase(0uy, 1uy, 0xA7uy, 8, 0xA7uy, 9, 0uy)>]
    [<TestCase(1uy, 1uy, 0xA7uy, 8, 0x53uy, 9, 0x80uy)>]
    member this.opDXYN_sipleSprite_correct(x,y,sprite, displayPosition1, result1, displayPosition2, result2) = 
        let emulator = Emulator()

        let code = [| 0xD0uy; 0x11uy|].AsSpan()

        emulator.initialize(code)

        //sprite to draw
        emulator.setVMMemory(0, sprite)
        emulator.setVMI(0us)

        //position to draw
        emulator.setVMVariable(0, x)
        emulator.setVMVariable(1, y)

        //draw
        emulator.tick()

        Assert.AreEqual(result1, emulator.display[displayPosition1])
        Assert.AreEqual(result2, emulator.display[displayPosition2])

    [<Test>]
    member this.opFX1E_correct() = 
        let emulator = Emulator()

        let code = [| 0xF0uy; 0x1Euy |].AsSpan()

        emulator.initialize(code)
        emulator.setVMI(10us)
        emulator.setVMVariable(0, 1uy)
        emulator.tick()

        Assert.AreEqual(11, emulator.i)
        
    [<Test>]
    member this.opFX29_correct() = 
        let emulator = Emulator()

        let code = [| 0xF0uy; 0x29uy |].AsSpan()

        emulator.initialize(code)
        emulator.setVMVariable(0, 5uy)
        emulator.tick()

        Assert.AreEqual(25us, emulator.i)

    [<TestCase(1uy, 0uy, 0uy, 1uy)>]
    [<TestCase(12uy, 0uy, 1uy, 2uy)>]
    [<TestCase(123uy, 1uy, 2uy, 3uy)>]
    member this.opFX33_correct(input:byte, result1:byte, result2:byte, result3:byte)=
        let emulator = Emulator()

        let code = [| 0xF0uy; 0x33uy |].AsSpan()
        emulator.initialize(code)
        emulator.setVMVariable(0, input)
        emulator.setVMMemory(0,0uy)
        emulator.setVMMemory(1,0uy)
        emulator.setVMMemory(2,0uy)
        emulator.setVMI(0us)
        emulator.tick()

        Assert.AreEqual(result1, emulator.memory[0])
        Assert.AreEqual(result2, emulator.memory[1])
        Assert.AreEqual(result3, emulator.memory[2])

    [<Test>]
    member this.opFX55_correct() = 
        let emulator = Emulator()

        let code = [| 0xFFuy; 0x55uy |].AsSpan()

        emulator.initialize(code)
        emulator.setVMI(1us)
        for i in 0 .. 15 do
            emulator.setVMVariable(i,10uy)

        emulator.tick()

        for i in 0 .. 15 do
            Assert.AreEqual(10us, emulator.memory[1+i])
        Assert.AreEqual(1, emulator.i)

    [<Test>]
    member this.opFX65_correct() = 
        let emulator = Emulator()

        let code = [| 0xFFuy; 0x65uy |].AsSpan()

        emulator.initialize(code)
        emulator.setVMI(1us)
        for i in 0 .. 15 do
            emulator.setVMMemory(1+i,10uy)

        emulator.tick()

        for i in 0 .. 15 do
            Assert.AreEqual(10us, emulator.variables[i])

        Assert.AreEqual(1, emulator.i)