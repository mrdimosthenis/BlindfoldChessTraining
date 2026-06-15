# Blindfold Chess Training

Blindfold Chess Training is a mobile application for Android and iOS designed to help chess players improve their visualization skills. The application allows users to practice blindfold chess by relying on memory and mental board visualization.

## Project Status

This project is currently inactive and is maintained here as a portfolio piece. Previously, the application was fully published and actively available on both the Google Play Store and the Apple App Store. It was eventually delisted due to the time constraints. The codebase remains functional and serves as a demonstration of functional programming in mobile development.

## Features

* Audio integration for playing moves and navigating the board without visual aids.
* Custom chess engine mechanics handling move validation and game state.
* Database of endgame puzzles and specific chess scenarios to practice visualization.
* Clean and modern user interface optimized for mobile devices.

## Tech Stack

* **Language:** F#
* **Framework:** .NET MAUI 
* **UI Architecture:** Fabulous (Declarative UI framework for F#)
* **Testing:** Custom codec tests for validating chess mechanics and endgame puzzles.

## Project Structure

* **BlindfoldChessMechanics:** Contains the core chess logic, move generation, and rules.
* **BlindfoldChessTest:** Houses unit tests and test cases for endgame puzzles.
* **Pages & UIElems:** Contains the UI components and screen definitions.
* **Speech.fs:** Handles the text to speech or voice recognition features.
* **DB.fs:** Manages local data storage and puzzle retrieval.

## Getting Started

To build and run this project locally, you will need the .NET SDK and the MAUI workloads installed.

1. Clone this repository.
2. Open the solution file `BlindfoldChessTraining.sln` in Visual Studio or JetBrains Rider.
3. Restore the NuGet packages.
4. Select your target emulator or physical device.
5. Build and deploy the project.
