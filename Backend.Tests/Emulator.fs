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
    member this.clearsScreen_correct() = 
        let emulator = Emulator()

        emulator.setVMMemory(emulator.displayMemoryShift, 0xFFuy)
        emulator.initialize([| 0x00uy; 0xE0uy |].AsSpan())
        emulator.tick()

        Assert.AreEqual(0x00uy, emulator.memory[emulator.displayMemoryShift])

    [<Test>]
    member this.jump_correct() = 
        let emulator = Emulator()

        emulator.initialize([| 0x16uy; 0x66uy |].AsSpan())
        emulator.tick()

        Assert.AreEqual(0x0666us, emulator.programCounter)

    [<Test>]
    member this.suroutineReturn_correct() = 
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
    member this.subroutineExecute_correct() = 
        let emulator = Emulator()

        let code = [| 0x26uy; 0x66uy |].AsSpan()

        emulator.initialize(code)
        emulator.tick()

        Assert.AreEqual(0x0666us, emulator.programCounter)
        Assert.AreEqual(1,emulator.stackPointer)
        Assert.AreEqual(0x02uy, emulator.stack[0])
        Assert.AreEqual(0x02uy, emulator.stack[1])