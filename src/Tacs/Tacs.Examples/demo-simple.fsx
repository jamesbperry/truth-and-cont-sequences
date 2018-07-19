#load "demo-refs.fsx"

open Tacs.Core
open Tacs.Core.Types
open Tacs.Core.Sequence
open Tacs.Core.IntegerOps
open Tacs.Core.FloatOps
open Tacs.Core.TimeOps
open Tacs.Examples

let ptvals = [{position=1;value=1.0};{position=2;value=2.0};{position=4;value=4.0};{position=5;value=5.0};{position=6;value=6.0}]
let ptseq = {id="test";extrap=BeforeAndAfter;bound=InclusiveLow;ptvalues=ptvals}

printfn "\n\nOriginal measurement points, e.g. from sensor:\n%A" ptseq
let intseq = Sequence.remodelPairwiseToIntervals IntegerPosition LinearFloatValue ptseq

printfn "\n\nRemodeled to intervals:\n%A" intseq
let ptseq2 = Sequence.remodelToPoints IntegerPosition InclusiveLow intseq

printfn "\n\nAfter a roundtrip to points, it is %A that original equals result" (ptseq = ptseq2)
let interpVal = Sequence.getPointInSequence intseq IntegerPosition 3

printfn "\n\nThe sequence interpolated at position 3 yields value %A" interpVal

let sliceStrat = { IntervalSlice.start=Some {boundary=Inclusive 2;strategy=Interpolated};endbound=Some {boundary=Inclusive 5;strategy=Interpolated}}   
let subseq = Sequence.sliceByInterval IntegerPosition sliceStrat intseq

printfn "\n\nAfter being sliced to between positions 2 and 5, the sequence is %A" subseq
let mean = FloatOps.Mean OnIntPosition subseq

printfn "\n\nThe sequence's time-weighted mean is %A" mean
let result = Sequence.remodelToPoints IntegerPosition InclusiveLow 
                { intseq with intvalues = mean }

printfn "\n\nRemodeled to points, this is %A" result

//TODO windowing once it's fixed