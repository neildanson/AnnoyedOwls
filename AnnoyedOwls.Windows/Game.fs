module AnnoyedOwls.Game

open System
open CocosSharp
open CocosDenshion
open FarseerPhysics.Dynamics
open FarseerPhysics.Factories
open Microsoft.Xna.Framework

let TileSize = 70.0f
let HalfTileSize = TileSize / 2.0f
let ScreenWidth = 1024.0f
let ScreenHeight = 600.0f
let HalfScreenWidth = ScreenWidth / 2.0f
let HalfScreenHeight = ScreenHeight / 2.0f
let ScreenSize = CCSize(ScreenWidth, ScreenHeight)
let intro = new CCTexture2D "Title"
let gameOverTexture = new CCTexture2D "GameOver"
let owl = new CCTexture2D "Owl"
let mouse = new CCTexture2D "Mouse"
let grass = new CCTexture2D "Grass"
let stone = new CCTexture2D "stone"
do CCSimpleAudioEngine.SharedEngine.PreloadEffect "Ow"

type Physics = 
    | Square of float32 * BodyType
    | Circle of float32 * BodyType

type NextState = 
    | GameOver of bool
    | NextLevel

type Entity = 
    | Prop
    | Enemy
    | Player
    | Meh

type Step = 
    | Update of float32
    | Collision of Body * Body

type Level = 
    { Props : string list
      BackgroundMusic : string
      Background : string list }

let blocks = 
    [ 'B', (grass, Square(TileSize, BodyType.Static))
      'S', (stone, Square(TileSize, BodyType.Static)) ]
    |> Map.ofList

let props = 
    [ 'b', (grass, Square(TileSize, BodyType.Dynamic))
      's', (stone, Square(TileSize, BodyType.Dynamic)) ]
    |> Map.ofList

let enemies = [ 'M', (mouse, Circle(HalfTileSize, BodyType.Dynamic)) ] |> Map.ofList

let makeBody world physics = 
    let body = 
        match physics with
        | Square(width, bodyType) -> 
            let body = BodyFactory.CreateRectangle(world, width / TileSize, width / TileSize, 1.0f)
            body.BodyType <- bodyType
            body
        | Circle(radius, bodyType) -> 
            let body = BodyFactory.CreateCircle(world, radius / TileSize, 1.0f)
            body.BodyType <- bodyType
            body
    body.Restitution <- 0.2f
    body.Friction <- 10.0f
    body.AngularDamping <- 1.0f
    body.IgnoreGravity <- false
    body

let (|Block|_|) (block, world) = 
    blocks
    |> Map.tryFind block
    |> Option.map (fun (texture, body) -> (makeBody world body), CCSprite(texture), Meh)

let (|PropBlock|_|) (block, world) = 
    props
    |> Map.tryFind block
    |> Option.map (fun (texture, body) -> (makeBody world body), CCSprite(texture), Prop)
    

let (|EnemyBlock|_|) (block, world) = 
    enemies
    |> Map.tryFind block
    |> Option.map (fun (texture, body) -> (makeBody world body), CCSprite(texture), Enemy)

let (|PlayerBlock|_|) (block, world) = 
    if block = 'O' then Some(makeBody world (Circle(HalfTileSize, BodyType.Dynamic)), CCSprite(owl), Player)
    else None

let addTouchControl (body:Body) (sprite:CCSprite) (layer:CCLayer) =
    let touchListener = new CCEventListenerTouchOneByOne()
    touchListener.OnTouchBegan <- fun x y -> 
        let mutable rect = sprite.BoundingBoxTransformedToWorld
        rect.Origin <- rect.Origin + layer.PositionWorldspace
        rect.ContainsPoint(x.Location)
    touchListener.OnTouchMoved <- fun x y -> 
        body.Position <- body.Position + Vector2(x.Delta.X / TileSize, x.Delta.Y / TileSize)
    touchListener.OnTouchEnded <- fun x y -> 
        let force = Vector2(x.Delta.X, x.Delta.Y)
        force.Normalize()

        body.ApplyForce(force * 1000.0f)
        sprite.RemoveEventListener(touchListener) 
    sprite.AddEventListener touchListener
    
let updatePositions entities = 
    entities |> List.iter (fun (body : Body, sprite : CCSprite, _) -> 
                    sprite.PositionX <- body.Position.X * TileSize
                    sprite.PositionY <- body.Position.Y * TileSize
                    sprite.Rotation <- -MathHelper.ToDegrees body.Rotation)

let handleCollisions (layer:CCLayer) (world:World) (entities:(Body * CCSprite * Entity) list) body1 body2 = 
    let t1 = entities |> List.tryFind (fun (b,_,_) -> b = body1)
    let t2 = entities |> List.tryFind (fun (b,_,_) -> b = body2)
    let toRemove = match t1, t2 with
                   | Some(b1,s1,e1), Some(b2,s2,e2) ->
                        match e1,e2 with
                        | Player, Enemy -> [b2,s2]
                        | Enemy, Player -> [b1,s1]
                        | Prop, Enemy -> [b2,s2]
                        | Enemy, Prop -> [b1,s1]
                        | _, _ -> []
                   | _,_ -> []

    toRemove |> List.iter (fun (body, sprite) -> world.RemoveBody body
                                                 let position = sprite.Position
                                                 sprite.RemoveFromParent true
                                                 let explosion = CCParticleExplosion(position)
                                                 layer.AddChild explosion)
    //if toRemove |> List.isEmpty |> not then 
    //    ignore <| CCSimpleAudioEngine.SharedEngine.PlayEffect "Ow"
    entities |> List.filter(fun (b,_,_) -> toRemove |> List.tryFind (fun (b',_) -> b = b') |> Option.isSome |> not)
    
let isWin entities =  entities |> List.exists (fun (_,_,entity) -> match entity with | Enemy -> true | _ -> false) |> not

let playLevel (level : Level) (window : CCWindow) = 
    let scene = CCScene window
    let layer = CCLayer()
    let world = World(Vector2(0.0f, -9.8f))

    let step = Event<_>()
    let stateMachine = Event<_>()

    world.FixtureAdded <- FixtureDelegate(
        fun fixture -> fixture.AfterCollision <- AfterCollisionEventHandler(fun body1 body2 _ _ -> step.Trigger(Collision(body1.Body, body2.Body))))
    
    let rec loop time entities = 
        async { 
            if time > 10.0f then 
                stateMachine.Trigger (GameOver(false))
            else 
                let! step = step.Publish |> Async.AwaitEvent
                match step with
                | Update ms ->                     
                    updatePositions entities
                    if isWin entities then
                        do! Async.Sleep 2000
                        stateMachine.Trigger NextLevel
                    return! loop (time + ms) entities
                | Collision(body1, body2) -> 
                    let entities = handleCollisions layer world entities body1 body2
                    return! loop time entities
        }
    


    level.Background
    |> List.rev
    |> List.map (fun background -> new CCTexture2D(background))
    |> List.map (fun texture -> 
           [ CCSprite(texture)
             CCSprite(texture)
             CCSprite(texture) ])
    |> List.mapi (fun i sprites -> 
           let node = CCParallaxNode()
           sprites 
           |> List.iteri 
                  (fun x sprite -> 
                  let x = float32 x
                  let ratio = 1.0f / (5.0f - float32 i)
                  node.AddChild
                      (sprite, i, CCPoint(ratio, 1.0f), 
                       CCPoint(x * sprite.ContentSize.Width + (sprite.ContentSize.Width / 2.0f), ScreenHeight / 2.0f)))
           node)
    |> List.iter (fun parallaxLayer -> layer.AddChild parallaxLayer)
    let entities = 
        level.Props
        |> Seq.mapi (fun y line -> 
               let y = (ScreenHeight / TileSize) - float32 y
               line |> Seq.mapi (fun x c -> 
                           match (c, world) with
                           | Block(body, sprite, entity) 
                           | PropBlock(body, sprite, entity)
                           | EnemyBlock(body, sprite, entity) -> 
                               body.Position <- Vector2(float32 x, float32 y)
                               layer.AddChild sprite
                               Some(body, sprite, entity)
                           | PlayerBlock(body, sprite, entity) -> 
                               body.Position <- Vector2(float32 x, float32 y)
                               layer.AddChild sprite
                               sprite.Schedule(fun _ -> layer.PositionX <- -sprite.PositionX + (float32 x * TileSize))
                               addTouchControl body sprite layer
                               Some(body, sprite, entity)
                           | _ -> None))
        |> Seq.collect id
        |> Seq.choose id
        |> Seq.toList
    updatePositions entities
    loop 0.0f entities |> Async.StartImmediate
    layer.Schedule(fun ms -> 
        world.Step ms
        step.Trigger(Update ms))

    //entities |> List.iter (fun (body, _, _) -> body.add_OnCollision(OnCollisionEventHandler(fun body1 body2 contacts -> 
    //                                                                        try
    //                                                                            step.Trigger(Collision(body1.Body, body2.Body))
    //                                                                        with _ -> ()
    //                                                                        true))) 
    scene.AddChild layer
    let transition = CCTransitionZoomFlipAngular(2.0f, scene, CCTransitionOrientation.DownOver)
    window.DefaultDirector.ReplaceScene transition
    CCSimpleAudioEngine.SharedEngine.PlayBackgroundMusic(level.BackgroundMusic)
    stateMachine.Publish |> Async.AwaitEvent

let introScreen (window : CCWindow) = 
    let scene = CCScene window
    let layer = CCLayerColor()
    CCSimpleAudioEngine.SharedEngine.PlayBackgroundMusic("Intro")
    scene.AddChild layer
    layer.AddChild(CCSprite(intro, PositionX = HalfScreenWidth, PositionY = HalfScreenHeight))
    let evt = Event<_>()
    let touchListener = new CCEventListenerTouchOneByOne()
    touchListener.OnTouchBegan <- fun _ _ -> evt.Trigger(); true
    scene.AddEventListener touchListener
    let transition = CCTransitionSplitCols(2.0f, scene)
    window.DefaultDirector.PushScene transition
    evt.Publish |> Async.AwaitEvent

let gameOver (window : CCWindow) success = 
    let scene = CCScene window
    let layer = CCLayer()
    CCSimpleAudioEngine.SharedEngine.PlayBackgroundMusic("GameOverMusic")
    scene.AddChild layer
    let gameOver = CCSprite(gameOverTexture, PositionX = HalfScreenWidth, PositionY = HalfScreenHeight)
    layer.AddChild gameOver
    if success then
        for x in 0.0f..300.0f..ScreenWidth do
            let firework = CCParticleFireworks(CCPoint(x, 0.0f))
            layer.AddChild firework
    
    let transition = CocosSharp.CCTransitionCrossFade(2.0f, scene)
    window.DefaultDirector.ReplaceScene transition
    Async.Sleep 3000

(*
Level Data
*)
let level1 = 
    { Props = 
          [ "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"; 
            "B                             B"; 
            "B                             B"; 
            "B          b                  B"; 
            "B          b                  B"; 
            "B          b                  B"; 
            "B          b                  B"; 
            "B   O    M b M                B"; 
            "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"; 
            "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB" ]
      BackgroundMusic = "Level1Music"
      Background = [ "BackgroundLayer1"; "BackgroundLayer2"; "BackgroundLayer3"; "BackgroundLayer4" ] }

let level2 = 
    { Props = 
          [ "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"; 
            "B              S              B"; 
            "B              S              B"; 
            "B                             B";
            "B                             B"; 
            "B              S              B"; 
            "B              S              B"; 
            "B   O          S       M      B"; 
            "SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS"; 
            "SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS" ]
      BackgroundMusic = "Level2Music"
      Background = [ "Level2Background1"; "Level2Background2"; "Level2Background3" ] }

let levels = [ level1; level2 ]

let rec runGame window = 
    async { 
        do! introScreen window
        let rec runLevel levels = 
            async { 
                match levels with
                | level :: levels -> 
                    let! nextState = playLevel level window
                    match nextState with
                    | NextLevel -> let! success = runLevel levels
                                   return success
                    | GameOver succeeded -> return succeeded
                | _ -> return true
            }
        let! success = runLevel levels
        do! gameOver window success
        return! runGame window
    }

type AppDelegate() = 
    inherit CCApplicationDelegate()
    override __.ApplicationDidFinishLaunching(app, window) = 
        app.ContentSearchPaths.Add "Content"
        CCScene.SetDefaultDesignResolution(ScreenWidth, ScreenHeight, CCSceneResolutionPolicy.ShowAll)
        runGame window |> Async.StartImmediate
        
    override __.ApplicationDidEnterBackground app = 
        CocosDenshion.CCSimpleAudioEngine.SharedEngine.PauseBackgroundMusic()
