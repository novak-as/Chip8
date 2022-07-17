open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open System
open Chip8Emulator

type Chip8 () as chip =
    inherit Game()

    let graphics = new GraphicsDeviceManager(chip)
    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>
    let emulator = Emulator()

    do
        chip.IsFixedTimeStep <- true
        chip.TargetElapsedTime <- TimeSpan.FromSeconds(1.0/60.0)

    override x.Initialize() =
    
        graphics.PreferredBackBufferWidth <- 960
        graphics.PreferredBackBufferHeight <- 480

        spriteBatch <- new SpriteBatch(x.GraphicsDevice)
        base.Initialize()

        let romName = "CONNECT4"
        let code = Chip8Emulator.loadProgramCode($"C:\\Users\\onovak\\Documents\\repos_personal\\chip8\\roms\\{romName}")
        emulator.initialize(code)

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
        let blockWidth = graphics.PreferredBackBufferWidth / (int)emulator.screenWidth
        let blockHeight = graphics.PreferredBackBufferHeight / (int)emulator.screenHeight

        let texture = Texture2D(chip.GraphicsDevice, 1,1)
        texture.SetData([| Color.White |])

        spriteBatch.Begin()
        let screen = emulator.display

        for x in 0 .. (int)emulator.screenWidth - 1 do
            for y in 0 .. (int)emulator.screenHeight - 1 do
                
                let isSet = emulator.getDisplayValue(x,y)

                let screenPositionX = x * blockWidth
                let screenPositionY = y * blockHeight

                spriteBatch.Draw(texture, Rectangle(screenPositionX, screenPositionY, blockWidth, blockHeight),
                            if isSet then Color.White else Color.Black)

        spriteBatch.End()

        ()


[<EntryPoint>]
let main argv = 
    use g = new Chip8()
    g.Run()
    0