module BlindfoldChessTraining.Preferences

open Xamarin.Essentials

// functions

let reset () = Preferences.Clear()

let removeIfExists k =
    if Preferences.ContainsKey(k) then
        Preferences.Remove(k)

let setBool k (v: bool) = Preferences.Set(k, v)

let setInt k (v: int) = Preferences.Set(k, v)

let setFloat k (v: float) = Preferences.Set(k, v)

let setFloat32 k (v: float32) = Preferences.Set(k, v)

let setString k (v: string) = Preferences.Set(k, v)

// keys

let isDisplayBoardOptionEnabledKey = "isDisplayBoardOptionEnabled"

let areCoordsEnabledKey = "areCoordsEnabled"
let areSymbolsEnabledKey = "areSymbolsEnabled"
let boardSizeKey = "boardSize"
let fontSizeRatioKey = "fontSizeRatio"
let selectedLocaleIndexKey = "selectedLocaleIndex"
let speechPitchKey = "speechPitch"

let didVolumeNoteClickedKey = "didVolumeNoteClicked"

let categoryIdKey = "categoryId"
let levelKey = "level"
let indexInLevelKey = "indexInLevel"

// values

let getIsDisplayBoardOptionEnabled () =
    Preferences.Get(isDisplayBoardOptionEnabledKey, true)

let getAreCoordsEnabled () =
    Preferences.Get(areCoordsEnabledKey, true)

let getAreSymbolsEnabled () =
    Preferences.Get(areSymbolsEnabledKey, false)

let getBoardSize () = Preferences.Get(boardSizeKey, 0.5)
let getFontSizeRatio () = Preferences.Get(fontSizeRatioKey, 1.0)

let getLocaleIndex () =
    if Preferences.ContainsKey(selectedLocaleIndexKey) then
        Some(Preferences.Get(selectedLocaleIndexKey, 0))
    else
        None

let getSpeechPitch () = Preferences.Get(speechPitchKey, 1.0f)

let getDidVolumeNoteClicked () =
    Preferences.Get(didVolumeNoteClickedKey, false)

let getCategoryId () = Preferences.Get(categoryIdKey, 0)
let getLevel () = Preferences.Get(levelKey, 0)
let getIndexInLevel () = Preferences.Get(indexInLevelKey, 0)
