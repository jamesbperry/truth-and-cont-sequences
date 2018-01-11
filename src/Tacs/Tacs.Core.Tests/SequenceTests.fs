module SequenceTests

open NUnit.Framework
open FsUnit
open Tacs.Core.Types
open Tacs.Core.Sequence

[<Test>]
let ``points to intervals should be reversible`` () =
    let ptvals = [{position=1;value=1};{position=2;value=2};{position=3;value=3};{position=4;value=4}]
    let ptseq = {id="test";extrap=ExtrapolationStrategy.BeforeAndAfter;interp=Linear;ptvalues=ptvals}
    let intseq = remodelToIntervals ptseq
    let ptseq2 = remodelToPoints RemodelAnchor.IntervalStart intseq
    ptseq2 |> should equal ptseq