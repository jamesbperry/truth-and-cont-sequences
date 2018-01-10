open Argu
open Arguments

let parseCli (parser:ArgumentParser<TacsArgs>) = 
    try
        let parsed = parser.ParseCommandLine(raiseOnUsage = true)
        Some parsed
    with
        | :? Argu.ArguParseException  -> None // return missing

let message (arguments:ParseResults<TacsArgs>) =
    let aggregate = arguments.GetResult <@ Aggregate @>
    let windowed = aggregate.GetResult <@ Windowed @>
    let sliding = windowed.GetResult <@ Sliding @>
    let size = sliding.GetResult <@ Of @>
    printfn "You said %s" size

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<TacsArgs>(programName = "tacs")
    let arguments = parseCli parser
    match arguments with
    | Some a -> message a; 0
    | None -> printfn "%s" (parser.PrintUsage()); 1