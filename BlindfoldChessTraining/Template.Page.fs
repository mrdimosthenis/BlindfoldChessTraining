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
    let header =
            View.StackLayout(
                orientation = StackOrientation.Horizontal,
                horizontalOptions = LayoutOptions.Center,
                verticalOptions = LayoutOptions.Start,
                children = [
                    Component.label model true title
                    View.Image(source = icon)
                ]
            )
    let backBtn = Component.button "Back" Icons.home false (fun () -> Model.HomePage |> Msg.SelectPage |> dispatch)
    let childElems = [ [ header ]
                       [ Component.separator() ]
                       innerElems
                       if model.SelectedPage = Model.HomePage
                            then []
                            else [ Component.separator(); backBtn ] ]
                     |> List.concat
    View.ContentPage(
        content = View.ScrollView(
            verticalOptions = LayoutOptions.Start,
            content = View.StackLayout(
                horizontalOptions = LayoutOptions.Center,
                verticalOptions = LayoutOptions.Center,
                children = childElems
            )
        )
    )
