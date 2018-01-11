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

let message (arguments:ParseResults<TacsArgs>) =
    let aggregate = arguments.GetResult <@ Arguments.Aggregate @>
    let windowed = aggregate.GetResult <@ Windowed @>
    let sliding = windowed.GetResult <@ Arguments.Sliding @>
    let size = sliding.GetResult <@ Of @>
    printfn "You said %s" size

let getEnvironmentCommandLineArgs () =
    match System.Environment.GetCommandLineArgs() with
    | [||] -> [||]
    | args -> args.[1..]

let devTests _ = 
    let vals = [{position=1;value=1};{position=2;value=2};{position=3;value=3};{position=4;value=4}]
    let ptseq = {id="42";interp=Linear;extrap=ExtrapolationStrategy.BeforeAndAfter;ptvalues=vals}
    let intseq = remodel ptseq
    printfn "%s" <| intseq.ToString()

let rec getArgsInteractive args =
    match System.Console.ReadLine() with
    | "" -> List.rev args
    | s -> getArgsInteractive (s :: args)

[<EntryPoint>]
let main argv =

    //testing
    //devTests()

    let parser = ArgumentParser.Create<TacsArgs>(programName = "tacs")

    let args = 
        match getEnvironmentCommandLineArgs() with
        | [||] ->
            printfn "Enter arguments on separate lines; blank to end"
            List.toArray <| getArgsInteractive []
        | a -> a

    printfn "Supplied arguments: %s" <| (List.ofArray args).ToString()    

    let arguments = 
        match args with
        | [||] -> Option<ParseResults<TacsArgs>>.None
        | _ -> parseCli parser args

    match arguments with
    | Some a -> message a; 
    | None -> printfn "%s" (parser.PrintUsage());

    0