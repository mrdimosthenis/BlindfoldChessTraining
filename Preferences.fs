module BlindfoldChessTraining.Preferences

open Microsoft.Maui.Storage

// functions

let reset () = Preferences.Clear()

// keys

let isDisplayBoardEnabledKey = "isDisplayBoardEnabled"

let areCoordsEnabledKey = "areCoordsEnabled"
let areSymbolsEnabledKey = "areSymbolsEnabled"
let boardSizeRatioKey = "boardSizeRatio"
let fontSizeRatioKey = "fontSizeRatio"
let localeIndexKey = "localeIndex"
let speechPitchKey = "speechPitch"

let didVolumeNoteClickedKey = "didVolumeNoteClicked"

let categoryIdKey = "categoryId"
let levelKey = "level"
let indexInLevelKey = "indexInLevel"

// getters

let getIsDisplayBoardEnabled () =
    Preferences.Get(isDisplayBoardEnabledKey, true)

let getAreCoordsEnabled () =
    Preferences.Get(areCoordsEnabledKey, true)

let getAreSymbolsEnabled () =
    Preferences.Get(areSymbolsEnabledKey, false)

let getBoardSizeRatio () =
    Preferences.Get(boardSizeRatioKey, 0.75)

let getFontSizeRatio () = Preferences.Get(fontSizeRatioKey, 1.)

let getLocaleIndex () =
    if Preferences.ContainsKey(localeIndexKey) then
        Some(Preferences.Get(localeIndexKey, 0))
    else
        None

let getSpeechPitch () = Preferences.Get(speechPitchKey, 1.f)

let getDidVolumeNoteClicked () =
    Preferences.Get(didVolumeNoteClickedKey, false)

let getCategoryId () = Preferences.Get(categoryIdKey, 0)
let getLevel () = Preferences.Get(levelKey, 0)
let getIndexInLevel () = Preferences.Get(indexInLevelKey, 0)

// setters

let setIsDisplayBoardEnabled (v: bool) =
    Preferences.Set(isDisplayBoardEnabledKey, v)

let setAreCoordsEnabled (v: bool) = Preferences.Set(areCoordsEnabledKey, v)

let setAreSymbolsEnabled (v: bool) =
    Preferences.Set(areSymbolsEnabledKey, v)

let setBoardSizeRatio (v: float) = Preferences.Set(boardSizeRatioKey, v)

let setFontSizeRatio (v: float) = Preferences.Set(fontSizeRatioKey, v)

let setLocaleIndex (v: int) = Preferences.Set(localeIndexKey, v)

let setSpeechPitch (v: float32) = Preferences.Set(speechPitchKey, v)

let setDidVolumeNoteClicked (v: bool) =
    Preferences.Set(didVolumeNoteClickedKey, v)

let setCategoryId (v: int) = Preferences.Set(categoryIdKey, v)
let setLevel (v: int) = Preferences.Set(levelKey, v)
let setIndexInLevel (v: int) = Preferences.Set(indexInLevelKey, v)
