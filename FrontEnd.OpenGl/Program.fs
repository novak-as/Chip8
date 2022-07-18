open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open System
open Chip8Emulator

type Chip8 () as chip =
    inherit Game()

    let graphics = new GraphicsDeviceManager(chip)
    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>
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
        chip.IsFixedTimeStep <- true
        chip.TargetElapsedTime <- TimeSpan.FromSeconds(1.0/60.0)

    override x.Initialize() =
    
        graphics.PreferredBackBufferWidth <- emulator.displayWidth
        graphics.PreferredBackBufferHeight <- emulator.displayHeight
        graphics.IsFullScreen <- true

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

        emulator.inputs.reset()
        for key in Keyboard.GetState().GetPressedKeys() do
            if keymap.ContainsKey(key) then
                emulator.inputs.set(byte(keymap[key]))


 
    override this.Draw (gameTime) =

        //TODO: this should be done only once
        let blockWidth = graphics.GraphicsDevice.Viewport.Width / emulator.displayWidth
        let blockHeight = graphics.GraphicsDevice.Viewport.Height / emulator.displayHeight

        let texture = Texture2D(chip.GraphicsDevice, 1,1)
        texture.SetData([| Color.White |])

        spriteBatch.Begin()
        let screen = emulator.display

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