open Argu
open Arguments

let parseCli (parser:ArgumentParser<TacsArgs>) args = 
    try
        let parsed = parser.ParseCommandLine(raiseOnUsage = true, inputs=args) 
        Some parsed
    with
        | :? Argu.ArguParseException  -> None // return missing

[<EntryPoint>]
let main argv =

    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some System.ConsoleColor.Red)
    let parser = ArgumentParser.Create<TacsArgs>(programName = "tacs",errorHandler=errorHandler)

    let results = parser.ParseCommandLine argv

    //TODO actually process the results
    printfn "Got parse results %A" <| results.GetAllResults ()

    0