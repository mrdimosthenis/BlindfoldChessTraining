module BlindfoldChessEngine.CustomScripts.TrainWithCsv

open FSharp.Data

open FSharpx.Collections
open Synapses
open BlindfoldChessEngine.CustomScripts

let learningRate = 0.01

let initNetwork: NeuralNetwork =
    Train.getLatestNetworkJson
    |> Utils.withDBTransaction 
    |> NeuralNetwork.ofJson


let testDataPoints = RandomNet.testDataPoints
let xs = LazyList.map fst testDataPoints
let ys = LazyList.map snd testDataPoints

let csvSeq =
    CsvFile.Load(
                    @"C:\Users\MrDIM\Desktop\lichess_2020_10_sample_202012041017.csv",
                    hasHeaders = true
                )

let calculatedRMSE (nn: NeuralNetwork): float =
    let predictions =
        LazyList.map (fun x -> NeuralNetwork.prediction(nn, x)) xs
    LazyList.zip ys predictions
    |> Statistics.rootMeanSquareError

let trainFromDataPoints(): unit =
    csvSeq.Rows
    |> Seq.map (fun row -> Train.encodedDatapoint(row.["pos"], row.["ev"]))
    |> Seq.indexed
    |> Seq.fold
            (fun acc (i, (x, y)) ->
                let nextNet = NeuralNetwork.fit(acc, learningRate, x, y)
                match i % 100 with
                | 0 ->
                    let rmse = calculatedRMSE nextNet
                    let nextNetJson = NeuralNetwork.toJson nextNet
                    Train.insertSingleNework(learningRate, rmse, nextNetJson)
                    |> Utils.withDBTransaction 
                | _ ->
                    ()
                nextNet
            )
            initNetwork
    |> ignore
