namespace BlindfoldChessTraining.WinUI

open System

module Program =
    [<EntryPoint; STAThread>]
    let main args =
        do FSharp.Maui.WinUICompat.Program.Main(args, typeof<BlindfoldChessTraining.WinUI.App>)
        0
