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
    
        graphics.PreferredBackBufferWidth <- 960
        graphics.PreferredBackBufferHeight <- 480

        spriteBatch <- new SpriteBatch(x.GraphicsDevice)
        base.Initialize()

        let romName = "test_opcode.ch8"
        emulator.load($"C:\\Users\\onovak\\Documents\\repos_personal\\chip8\\roms\\{romName}")

        ()

    override this.Update (gameTime) =
        try
            emulator.tick()
         with 
            | ex -> 
                printfn $"{ex.Message}"
                printfn $"{ex.InnerException}"

                createMemoryDump(emulator.memory)
                exit(1)

        ()
 
    override this.Draw (gameTime) =

        //TODO: this should be done only once
        let blockWidth = graphics.PreferredBackBufferWidth / emulator.screenWidth
        let blockHeight = graphics.PreferredBackBufferHeight / emulator.screenHeight

        let texture = Texture2D(chip.GraphicsDevice, 1,1)
        texture.SetData([| Color.White |])

        spriteBatch.Begin()
        for x in 0 .. emulator.screenWidth-1 do
            for y in 0 .. emulator.screenHeight-1 do
                let status = emulator.screen[x,y]
                spriteBatch.Draw(texture, Rectangle(x*blockWidth, y*blockHeight, blockWidth, blockHeight),
                    if status then Color.White else Color.Black)
        spriteBatch.End()

        ()


[<EntryPoint>]
let main argv = 
    use g = new Chip8()
    g.Run()
    0