﻿module BlindfoldChessTraining.Update

open System.Diagnostics
open Fabulous
open Fabulous.XamarinForms
open Fabulous.XamarinForms.LiveUpdate
open Xamarin.Forms

let timerCmd : Cmd<Msg.Msg> =
    async { do! Async.Sleep 200
            return Msg.TimedTick }
    |> Cmd.ofAsyncMsg

let update (msg: Msg.Msg) (model: Model.Model): Model.Model * Cmd<Msg.Msg> =
    match msg with
    | Msg.Increment -> { model with Model.Count = model.Count + model.Step }, Cmd.none
    | Msg.Decrement -> { model with Model.Count = model.Count - model.Step }, Cmd.none
    | Msg.Reset -> Model.init, Cmd.none
    | Msg.SetStep n -> { model with Step = n }, Cmd.none
    | Msg.TimerToggled on -> { model with TimerOn = on }, (if on then timerCmd else Cmd.none)
    | Msg.TimedTick -> 
        if model.TimerOn then 
            { model with Count = model.Count + model.Step }, timerCmd
        else 
            model, Cmd.none
    | Msg.SelectPage v -> { model with Model.SelectedPage = v }, Cmd.none