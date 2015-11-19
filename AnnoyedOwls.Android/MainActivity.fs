namespace AnnoyedOwls.Android

open System

open Android.App
open Android.Content
open Android.Content.PM
open Android.OS
open Android.Runtime
open Android.Views
open Android.Widget
open Microsoft.Xna.Framework
open CocosSharp
open System
open AnnoyedOwls
open AnnoyedOwls.Game

[<Activity (Label = "AnnoyedOwls.Android", MainLauncher = true, ScreenOrientation = ScreenOrientation.Landscape, ConfigurationChanges = (ConfigChanges.Orientation ||| ConfigChanges.ScreenSize))>]
type MainActivity () =
    inherit AndroidGameActivity ()
    override this.OnCreate bundle =
        base.OnCreate bundle
        let app = new CCApplication(false, Nullable(CCSize(1024.0f, 600.0f)))
        app.ApplicationDelegate <- AppDelegate()
        base.SetContentView(app.AndroidContentView)
        app.StartGame()