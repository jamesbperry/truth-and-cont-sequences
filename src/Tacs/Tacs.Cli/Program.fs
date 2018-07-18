open Argu
open Arguments
open Newtonsoft.Json
open Tacs.Core
open Tacs.Core.Types

let readAllStdin () =
    Seq.takeWhile (function null -> false | _ -> true) <| Seq.initInfinite (fun _ -> System.Console.ReadLine()) 

let readPointsFromJson<'p,'v> () =
    let textin =  String.concat " " <| readAllStdin ()
    JsonConvert.DeserializeObject<PointValue<'p,'v> list> textin

type ParserBuilder() =
    member __.Bind(x,f) =
        match x with
        | None -> None
        | Some s -> f s
    member __.Return(x) =
        Some x    

let parser = new ParserBuilder()   

//Replace with impl from FSharp.Data (?)
let readPointsFromCsv<'p,'v> (posparse:string->'p option) (valparse:string->'v option) =
    let splitLine (line:string) =
        let toks = line.Split ","
        match toks with
        | [|p;v|] -> Some (p,v)
        | _ -> None //failwith "invalid line encountered"
    let tokensin = readAllStdin () |> Seq.where (System.String.IsNullOrWhiteSpace >> not) |> Seq.map splitLine |> Seq.choose id
    let parseToks (ptext:string,vtext:string) =
        parser
            {
            let! p = posparse ptext
            let! v = valparse vtext
            return (p,v)
            }      
    tokensin |> Seq.map parseToks |> Seq.choose id

let tryParseAs tryParser = tryParser >> function
        | true, v -> Some v
        | false, _ -> None

let parseAsFloat = tryParseAs System.Double.TryParse
let parseAsInt = tryParseAs System.Int32.TryParse
let parseAsTime = tryParseAs System.DateTimeOffset.TryParse

let remodelDemo rargs =
    //Placeholder for demo only
    printfn "Enjoy this demo"
    let pttups = readPointsFromCsv parseAsTime parseAsFloat
    let ptvals = pttups |> Seq.map (fun (p,v) -> {position=p;value=v}) |> List.ofSeq
    let ptseq = {id="test";extrap=BeforeAndAfter;bound=InclusiveLow;ptvalues=ptvals}
    let intseq = Sequence.remodelPairwiseToIntervals TimeOps.TimePosition FloatOps.LinearFloatValue ptseq
    printf "%A" intseq

[<EntryPoint>]
let main argv =

    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some System.ConsoleColor.Red)
    let argParser = ArgumentParser.Create<TacsArgs>(programName = "tacs",errorHandler=errorHandler)
    let pargs = argParser.ParseCommandLine argv

    let targs = pargs.GetAllResults ()
    printfn "Got parse results %A" <| targs
    
    match List.head targs with
        | Remodel r -> remodelDemo r
        | _ -> ()

    0