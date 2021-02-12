module BlindfoldChessTraining.Template.Page

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

open BlindfoldChessTraining
open BlindfoldChessTraining.UIElems

let page (model: Model.Model)
         (dispatch: Msg.Msg -> unit)
         (title: string)
         (icon: Image.Value)
         (innerElems: ViewElement list)
         : ViewElement =
    let backBtn =
        let icon =
            if model.SelectedPage = Model.HomePage then Icons.exit else Icons.play_left

        fun () -> dispatch Msg.BackPressed
        |> Component.imageButton icon

    let header =
        View.StackLayout
            (orientation = StackOrientation.Horizontal,
             horizontalOptions = LayoutOptions.Center,
             verticalOptions = LayoutOptions.Start,
             children =
                 [ backBtn
                   Component.label model true title
                   View.Image(source = icon) ])

    let childElems =
        List.append [ header; Component.separator () ] innerElems

    View.ContentPage
        (backgroundColor = Constants.backgroundColor,
         content =
             View.ScrollView
                 (verticalOptions = LayoutOptions.Start,
                  content =
                      View.StackLayout
                          (horizontalOptions = LayoutOptions.Center,
                           verticalOptions = LayoutOptions.Center,
                           children = childElems)))
