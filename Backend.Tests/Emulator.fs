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

        emulator.do_clearScreen()

        Assert.AreEqual(0x00uy, emulator.memory[emulator.displayMemoryShift])

    [<Test>]
    member this.jump_correct() = 
        let emulator = Emulator()

        emulator.do_jump(0x0666us)

        Assert.AreEqual(0x0666us, emulator.programCounter)

    [<Test>]
    member this.subroutine_correct() = 
        let emulator = Emulator()

        let code = [| 0x26uy; 0x66uy |].AsSpan()

        emulator.initialize(code)

        let pc = emulator.programCounter
        emulator.tick()

        Assert.AreEqual(0x0666us, emulator.programCounter)
        Assert.AreEqual(1,emulator.stackPointer)
        Assert.AreEqual(pc+2us, emulator.stack[0])

    [<Test>]
    member this.return_correct() = 
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
