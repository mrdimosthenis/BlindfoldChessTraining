module BlindfoldChessTraining.Page.Example

open System.Diagnostics
open Fabulous
open Fabulous.XamarinForms
open Fabulous.XamarinForms.LiveUpdate
open Xamarin.Forms

open BlindfoldChessTraining

let view (model: Model.Model) (dispatch: Msg.Msg -> unit): ViewElement =
    View.ContentPage(
        content = View.StackLayout(padding = Thickness 20.0, verticalOptions = LayoutOptions.Center,
          children = [ 
              View.Label(text = sprintf "%d" model.Count, horizontalOptions = LayoutOptions.Center, width=200.0, horizontalTextAlignment=TextAlignment.Center)
              View.Button(text = "Increment", command = (fun () -> dispatch Msg.Increment), horizontalOptions = LayoutOptions.Center)
              View.Button(text = "Decrement", command = (fun () -> dispatch Msg.Decrement), horizontalOptions = LayoutOptions.Center)
              View.Label(text = "Timer", horizontalOptions = LayoutOptions.Center)
              View.Switch(isToggled = model.TimerOn, toggled = (fun on -> dispatch (Msg.TimerToggled on.Value)), horizontalOptions = LayoutOptions.Center)
              View.Slider(minimumMaximum = (0.0, 10.0), value = double model.Step, valueChanged = (fun args -> dispatch (Msg.SetStep (int (args.NewValue + 0.5)))), horizontalOptions = LayoutOptions.FillAndExpand)
              View.Label(text = sprintf "Step size: %d" model.Step, horizontalOptions = LayoutOptions.Center) 
              View.Button(text = "Reset", horizontalOptions = LayoutOptions.Center, command = (fun () -> dispatch Msg.Reset), commandCanExecute = (1 <> 1))
              View.Button(text = "Back", horizontalOptions = LayoutOptions.Center, command = fun () -> dispatch (Msg.SelectPage Model.HomePage))
          ]
    )
)
