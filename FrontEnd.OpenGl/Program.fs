open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open System
open Chip8Emulator

type Chip8 () as chip =
    inherit Game()

    let graphics = new GraphicsDeviceManager(chip)
    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>
    let emulator = Emulator()

    override x.Initialize() =
    
        spriteBatch <- new SpriteBatch(x.GraphicsDevice)
        base.Initialize()

        let romName = "octojam1title.ch8"
        emulator.load($"C:\\Users\\onovak\\Documents\\repos_personal\\chip8\\roms\\{romName}")

        ()

    override this.Update (gameTime) =
        try
            emulator.tick()
         with 
            | ex -> 
                let (commandByte1, commandByte2) = emulator.command
                let message = $"Unable to process with opcode {commandByte1:X2} {commandByte2:X2} at position {emulator.programCounter-2s}"
                createMemoryDump(emulator.memory)
                raise (Exception(message, ex))

        ()
 
    override this.Draw (gameTime) =

        chip.GraphicsDevice.Clear Color.Black
        

        ()


[<EntryPoint>]
let main argv = 
    use g = new Chip8()
    g.Run()
    0