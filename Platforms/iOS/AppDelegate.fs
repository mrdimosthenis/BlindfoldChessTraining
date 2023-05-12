namespace BlindfoldChessTraining

open BlindfoldChessTraining.Types
open Foundation
open Microsoft.Maui

type KeyCodeReceivedService() =
    let keyCodeReceived = Event<KeyCodeResult>()

    static let _instance = KeyCodeReceivedService()

    static member Current = _instance

    member this.OnKeyCodeReceivedReceived(code: KeyCodeResult) = keyCodeReceived.Trigger(code)

    interface IKeyCodeReceivedService with
        [<CLIEvent>]
        member this.KeyCodeReceived = keyCodeReceived.Publish

[<Register("AppDelegate")>]
type AppDelegate() =
    inherit MauiUIApplicationDelegate()

    do CodeReceivedService.Instance <- KeyCodeReceivedService.Current

    override _.CreateMauiApp() = MauiProgram.CreateMauiApp()
