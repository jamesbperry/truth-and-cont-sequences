namespace Tacs.Core

module TimeOps =

    open Types
    open System

    let inline ClampScale (v:'a) =
        min LanguagePrimitives.GenericOne (max LanguagePrimitives.GenericZero v)

    let PositionScale (low:System.DateTimeOffset) (high:System.DateTimeOffset) (at:System.DateTimeOffset) =
        ClampScale <| (float (at.Ticks - low.Ticks))/(float (high.Ticks - low.Ticks))                   

    let InterpolatePosition (iv:FiniteIntervalValue<DateTimeOffset,'v>) p : float =
        PositionScale iv.start iv.``end`` p

    let InterpolateValueConstant (v:DateTimeOffset) (npos:NormalizedPosition)  =
        v //TODO clamp to None if position is out of bounds
        
    let InterpolateValueLinear (bounds:DateTimeOffset * DateTimeOffset) (npos:NormalizedPosition) =
        let (vi, vf) = bounds
        let dv = vf.Subtract vi
        let (NormalizedPosition nposu) = npos
        let f = TimeSpan.FromTicks <| int64 (float dv.Ticks * nposu)
        vi + f

    //Integer
    let Integrate (inseq:IntervalValue<'a,DateTimeOffset> seq) : (IntervalValue<'a,DateTimeOffset>) =
        failwith "not implemented"
    
    let Average (inseq:IntervalValue<'a,DateTimeOffset> seq) : (IntervalValue<'a,DateTimeOffset>) =
        failwith "not implemented"

    let Maximum (inseq:IntervalValue<'a,DateTimeOffset> seq) : (IntervalValue<'a,DateTimeOffset>) =
        failwith "not implemented"

    let Minimum (inseq:IntervalValue<'a,DateTimeOffset> seq) : (IntervalValue<'a,DateTimeOffset>) =
        failwith "not implemented"

    let Stdev (inseq:IntervalValue<'a,DateTimeOffset> seq) : (IntervalValue<'a,DateTimeOffset>) =
        failwith "not implemented"

    let ValueRange (inseq:IntervalValue<'a,DateTimeOffset> seq) : (IntervalValue<'a,DateTimeOffset>) =
        failwith "not implemented"
                      