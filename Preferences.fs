﻿module BlindfoldChessTraining.Preferences

open Microsoft.Maui.Storage

// keys

let isDisplayBoardEnabledKey = "isDisplayBoardEnabled"

let areCoordsEnabledKey = "areCoordsEnabled"
let areSymbolsEnabledKey = "areSymbolsEnabled"
let boardSizeRatioKey = "boardSizeRatio"
let fontSizeRatioKey = "fontSizeRatio"
let localeIndexKey = "localeIndex"
let speechPitchKey = "speechPitch"

let didSpeakInPuzzleKey = "didSpeakInPuzzle"

let levelEndgameKey = "levelEndgame"
let indexInLevelEndgameKey = "indexInLevelEndgame"
let levelOpeningKey = "level"
let indexInLevelOpeningKey = "indexInLevelOpening"

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

let getLocaleIndex () = Preferences.Get(localeIndexKey, 0)

let getSpeechPitch () = Preferences.Get(speechPitchKey, 1.f)

let getDidSpeakInPuzzle () =
    Preferences.Get(didSpeakInPuzzleKey, false)

let getLevelEndgame () = Preferences.Get(levelEndgameKey, 0)

let getIndexInLevelEndgame () =
    Preferences.Get(indexInLevelEndgameKey, 0)

let getLevelOpening () = Preferences.Get(levelOpeningKey, 0)

let getIndexInLevelOpening () =
    Preferences.Get(indexInLevelOpeningKey, 0)

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

let setDidSpeakInPuzzle (v: bool) = Preferences.Set(didSpeakInPuzzleKey, v)

let setLevelEndgame (v: int) = Preferences.Set(levelEndgameKey, v)

let setIndexInLevelEndgame (v: int) =
    Preferences.Set(indexInLevelEndgameKey, v)

let setLevelOpening (v: int) = Preferences.Set(levelOpeningKey, v)

let setIndexInLevelOpening (v: int) =
    Preferences.Set(indexInLevelOpeningKey, v)

// reset functions

let reset () = Preferences.Clear()

let resetConfig () =
    let levelEndgame = getLevelEndgame ()
    let indexInLevelEndgame = getIndexInLevelEndgame ()
    let levelOpening = getLevelOpening ()
    let indexInLevelOpening = getIndexInLevelOpening ()
    reset ()
    setLevelEndgame levelEndgame
    setIndexInLevelEndgame indexInLevelEndgame
    setLevelOpening levelOpening
    setIndexInLevelOpening indexInLevelOpening
