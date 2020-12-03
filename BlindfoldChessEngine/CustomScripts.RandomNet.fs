module BlindfoldChessEngine.CustomScripts.RandomNet

open FSharpx.Collections

open Synapses
open Npgsql
open BlindfoldChessEngine.CustomScripts

let randIntCeil(n: int): int =
    System.Random().NextDouble()
    |> (*) (float n)
    |> ceil
    |> int

let randLayers(): int list =
    let hiddenLayers =
        BlindfoldChessMechanics.Utils.lazInfinite
        |> LazyList.take (randIntCeil 4)
        |> LazyList.map (fun _ -> randIntCeil 600)
        |> LazyList.toList
    [ [ 782 ]; hiddenLayers; [ 3 ] ]
    |> List.concat

let activationF () (layerIndex: int)
    : ActivationFunction =
    ActivationFunction.sigmoid

let weightInitF () (layerIndex: int): float =
    let factor =
        [| randIntCeil(4)
           randIntCeil(4)
           randIntCeil(4)
           randIntCeil(4)
           randIntCeil(4)
           randIntCeil(4)
           randIntCeil(4)
           randIntCeil(4) |].[layerIndex]
        |> float
    System.Random().NextDouble()
    |> (-) 0.5
    |> (*) factor

let randNet(): NeuralNetwork =
    NeuralNetwork.customizedInit(
        randLayers(),
        activationF(),
        weightInitF()
    )
    
let getDataPoints (n: int): (float list * float list) LazyList =
    let f (conn: NpgsqlConnection, trans: NpgsqlTransaction) =
        Train.getRandomDataPoints n (conn, trans)
        |> LazyList.ofList
        |> LazyList.map Train.encodedDatapoint
    Utils.withDBTransaction f

let trainDataPoints: (float list * float list) LazyList =
    getDataPoints 500

let testDataPoints: (float list * float list) LazyList =
    getDataPoints 100

let createFitAndInsertNet(): unit =
    let f (conn: NpgsqlConnection, trans: NpgsqlTransaction) =
        let learningRate = 0.5 //System.Random().NextDouble()
        let network = randNet()
        let trainedNetwork =
            trainDataPoints
            |> LazyList.fold
                    (fun acc (x, y) ->
                        NeuralNetwork.fit(acc, learningRate, x, y)
                    )
                    network
        let xs = LazyList.map fst testDataPoints
        let ys = LazyList.map snd testDataPoints
        let predictions =
            LazyList.map (fun x -> NeuralNetwork.prediction(trainedNetwork, x)) xs
        let rmse = LazyList.zip ys predictions
                   |> Statistics.rootMeanSquareError
        let trainedNetworkJson = NeuralNetwork.toJson trainedNetwork
        Train.insertSingleNework(learningRate, rmse, trainedNetworkJson)(conn, trans)
    try
        Utils.withDBTransaction f
    with
    | _ -> ()

let rec infiniteCreateFitAndInsertNet(): unit =
    createFitAndInsertNet() |> ignore
    infiniteCreateFitAndInsertNet()
