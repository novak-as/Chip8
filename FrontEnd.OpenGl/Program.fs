﻿open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open System
open System.IO
open NLog
open Chip8Emulator

let logger = LogManager.GetCurrentClassLogger()

type Chip8(path, romName) as this =
    inherit Game()

    let _path = path
    let _romName = romName

    let graphics = new GraphicsDeviceManager(this)
    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>

    let mutable texture:Texture2D = null
    let mutable blockSize = Point.Zero
    let mutable margin = Point.Zero

    let emulator = Emulator()

    //ORIGINAL CHIP KEYBOAD
    //1 2 3 C
    //4 5 6 D
    //7 8 9 E
    //A 0 B F
    let keymap = dict[
        Keys.D1, 1;
        Keys.D2, 2;
        Keys.D3, 3;
        Keys.D4, 12;

        Keys.Q, 4;
        Keys.W, 5;
        Keys.E, 6;
        Keys.R, 13;

        Keys.A, 7;
        Keys.S, 8;
        Keys.D, 9;
        Keys.F, 14;

        Keys.Z, 10;
        Keys.X, 0;
        Keys.C, 11;
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

        blockSize <- Point((graphics.GraphicsDevice.Viewport.Width / emulator.displayWidth),
                            (graphics.GraphicsDevice.Viewport.Height / emulator.displayHeight))
        margin <- Point((graphics.GraphicsDevice.Viewport.Width - blockSize.X * emulator.displayWidth) / 2,
                         (graphics.GraphicsDevice.Viewport.Height - blockSize.Y * emulator.displayHeight) / 2)

        spriteBatch <- new SpriteBatch(x.GraphicsDevice)
        base.Initialize()

        let code = Chip8Emulator.loadProgramCode (Path.Combine [| _path; _romName |])
        emulator.initialize(code)
        logger.Info($"ROM has been loaded")

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
                
                let isSet = emulator.display.getUnpackedValue x y

                let screenPosition = Point(int(Math.Round(float(x) * float(blockSize.X))), 
                                             int(Math.Round(float(y) * float(blockSize.Y))))

                spriteBatch.Draw(texture, Rectangle(screenPosition + margin, blockSize), if isSet then Color.White else Color.Black)

        spriteBatch.End()

        ()
        

let parseArgs args =

    let tail2 list = list |> List.tail |> List.tail

    let rec parseArgsInternal args path romName = 
        match List.length(args) with 
            | 1 -> (path, args[0])
            | _ -> match args[0] with
                | "--path" -> parseArgsInternal (tail2 args) args[1] romName
                | _ -> raise (Exception($"Invalid argument {args[0]}"))

    parseArgsInternal args "roms" String.Empty
        


[<EntryPoint>]
let main argv = 

    let args = Environment.GetCommandLineArgs () |> Array.toList |> List.tail

    let (path, romName) = parseArgs args
    logger.Info $"Path: {path}, Rom: {romName}"

    use g = new Chip8(path, romName)
    g.Run()
    0