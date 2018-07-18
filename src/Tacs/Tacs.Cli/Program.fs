open Argu
open Arguments
open Tacs.Core.Sequence
open Tacs.Core.Types

let parseCli (parser:ArgumentParser<TacsArgs>) args = 
    try
        let parsed = parser.ParseCommandLine(raiseOnUsage = true, inputs=args) 
        Some parsed
    with
        | :? Argu.ArguParseException  -> None // return missing

// let message (arguments:ParseResults<TacsArgs>) =
//     let aggregate = arguments.GetResult <@ Arguments.Aggregate @>
//     let windowed = aggregate.GetResult <@ Windowed @>
//     let sliding = windowed.GetResult <@ Arguments.Sliding @>
//     let size = sliding.GetResult <@ Of @>
//     printfn "You said %s" size

let getEnvironmentCommandLineArgs () =
    match System.Environment.GetCommandLineArgs() with
    | [||] -> [||]
    | args -> args.[1..]

let rec getArgsInteractive args =
    match System.Console.ReadLine() with
    | "" -> List.rev args
    | s -> getArgsInteractive (s :: args)

[<EntryPoint>]
let main argv =

    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some System.ConsoleColor.Red)
    let parser = ArgumentParser.Create<TacsArgs>(programName = "tacs",errorHandler=errorHandler)


    let results = parser.ParseCommandLine argv

    printfn "Got parse results %A" <| results.GetAllResults ()

    0