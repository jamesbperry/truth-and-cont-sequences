// Learn more about F# at http://fsharp.org

open Argu
open Arguments

let parseCli (parser:ArgumentParser<TacsArgs>) = 
    try
        let parsed = parser.ParseCommandLine(raiseOnUsage = true)
        Some parsed
    with
        | :? Argu.ArguParseException  -> None // return missing

let message (arguments:ParseResults<TacsArgs>) =
    let slice = arguments.Contains <@ Slice @>
    if (slice) then printfn "Slice'em and dice'em"

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<TacsArgs>(programName = "tacs")
    let arguments = parseCli parser
    match arguments with
    | Some a -> message a; 0
    | None -> printfn "%s" (parser.PrintUsage()); 1