open Argu
open Arguments
open Tacs.Core.Sequence
open Tacs.Core.Types

let parseCli (parser:ArgumentParser<TacsArgs>) = 
    try
        let parsed = parser.ParseCommandLine(raiseOnUsage = true)
        Some parsed
    with
        | :? Argu.ArguParseException  -> None // return missing

let message (arguments:ParseResults<TacsArgs>) =
    let aggregate = arguments.GetResult <@ Arguments.Aggregate @>
    let windowed = aggregate.GetResult <@ Windowed @>
    let sliding = windowed.GetResult <@ Arguments.Sliding @>
    let size = sliding.GetResult <@ Of @>
    printfn "You said %s" size

let devTests _ = 
    let vals = [{position=1;value=1};{position=2;value=2};{position=3;value=3};{position=4;value=4}]
    let ptseq = {id="42";interp=Linear;extrap=ExtrapolationStrategy.BeforeAndAfter;ptvalues=vals}
    let intseq = remodel ptseq
    printfn "%s" <| intseq.ToString()

[<EntryPoint>]
let main argv =

    //testing
    devTests()

    let parser = ArgumentParser.Create<TacsArgs>(programName = "tacs")
    let arguments = parseCli parser
    match arguments with
    | Some a -> message a; 
    | None -> printfn "%s" (parser.PrintUsage());

    0