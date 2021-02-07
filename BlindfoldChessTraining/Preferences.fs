module BlindfoldChessTraining.Preferences

open Xamarin.Essentials

// preferences keys

let isDisplayBoardOptionEnabledKey: string = "isDisplayBoardOptionEnabled"

let areCoordsEnabledKey: string = "areCoordsEnabled"
let boardSizeKey: string = "boardSize"
let areSymbolsEnabledKey: string = "areSymbolsEnabled"
let fontSizeKey: string = "fontSize"
let selectedLocaleKey: string = "selectedLocale"
let speechPitchKey: string = "speechPitch"

let didVolumeNoteClickedKey: string = "didVolumeNoteClicked"

let endgameJsonStrKey: string = "endgameJsonStr"
let openingJsonStrKey: string = "openingJsonStr"

let areAnalyticsEnabledKey: string = "areAnalyticsEnabled"
let userIdKey: string = "userId"

// functions

let clear(): unit =
    Preferences.Clear()

let removeIfExists (k: string): unit =
    if Preferences.ContainsKey(k) then Preferences.Remove(k)

let setBool (k: string) (v: bool): unit =
    Preferences.Set(k, v)

let setInt (k: string) (v: int): unit =
   Preferences.Set(k, v)

let setFloat (k: string) (v: float): unit =
   Preferences.Set(k, v)

let setString (k: string) (v: string): unit =
   Preferences.Set(k, v)

let tryGetBool (k: string): bool option =
    if Preferences.ContainsKey(k) then Some (Preferences.Get(k, false))
    else None

let tryGetInt (k: string): int option =
   if Preferences.ContainsKey(k) then Some (Preferences.Get(k, 0))
   else None

let tryGetFloat (k: string): float option =
   if Preferences.ContainsKey(k) then Some (Preferences.Get(k, 0.0))
   else None

let tryGetString (k: string): string option =
   if Preferences.ContainsKey(k) then Some (Preferences.Get(k, ""))
   else None
