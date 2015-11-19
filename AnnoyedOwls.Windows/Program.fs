open CocosSharp
open System
open AnnoyedOwls
open AnnoyedOwls.Game

let run() = 
    let app = new CCApplication(false, Nullable(CCSize(1024.0f, 600.0f)))
    app.ApplicationDelegate <- AppDelegate()
    app.StartGame()

run()
//CocosDenshion.CCSimpleAudioEngine.SharedEngine.StopBackgroundMusic()
    