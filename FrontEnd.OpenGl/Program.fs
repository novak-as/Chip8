open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open System
open NLog
open Chip8Emulator

let logger = LogManager.GetCurrentClassLogger()

type Chip8 () as this =
    inherit Game()

    let graphics = new GraphicsDeviceManager(this)
    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>

    let mutable texture:Texture2D = null
    let mutable blockWidth = 0
    let mutable blockHeight = 0

    let emulator = Emulator()
    let keymap = dict[
        Keys.D1, 0;
        Keys.D2, 1;
        Keys.D3, 2;
        Keys.D4, 3;

        Keys.Q, 4;
        Keys.W, 5;
        Keys.F, 6;
        Keys.P, 7;

        Keys.A, 8;
        Keys.R, 9;
        Keys.S, 10;
        Keys.T, 11;

        Keys.Z, 12;
        Keys.X, 13;
        Keys.C, 14;
        Keys.V, 15;
    ]

    do
        this.IsFixedTimeStep <- true
        this.TargetElapsedTime <- TimeSpan.FromSeconds(1.0/60.0)

    override x.Initialize() =
    
        graphics.PreferredBackBufferWidth <- emulator.displayWidth
        graphics.PreferredBackBufferHeight <- emulator.displayHeight
        graphics.IsFullScreen <- true

        
        texture <- Texture2D(this.GraphicsDevice, 1,1)
        texture.SetData([| Color.White |])

        blockWidth <- graphics.GraphicsDevice.Viewport.Width / emulator.displayWidth
        blockHeight <- graphics.GraphicsDevice.Viewport.Height / emulator.displayHeight 

        spriteBatch <- new SpriteBatch(x.GraphicsDevice)
        base.Initialize()

        let romName = "hidden"
        let code = Chip8Emulator.loadProgramCode($"C:\\Users\\onovak\\Documents\\repos_personal\\chip8\\roms\\{romName}")
        emulator.initialize(code)
        logger.Info($"ROM '{romName}' was loaded")

        ()

    override this.Update (gameTime) =
        try
            emulator.delayTimer.tick()
            emulator.soundTimer.tick()

            emulator.tick()
         with 
            | ex -> 
                logger.Fatal $"{ex.Message}"
                logger.Fatal $"{ex.InnerException}"

                createMemoryDump(emulator.memory)
                exit(1)

        emulator.inputs.reset()
        for key in Keyboard.GetState().GetPressedKeys() do
            if keymap.ContainsKey(key) then
                emulator.inputs.set(byte(keymap[key]))


 
    override this.Draw (gameTime) =

        spriteBatch.Begin()

        for x in 0 .. int(emulator.displayWidth) - 1 do
            for y in 0 .. int(emulator.displayHeight) - 1 do
                
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