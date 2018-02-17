namespace Tacs.Core

module TimeOps =

    open Types
    open System

    let inline ClampScale (v:'a) =
        min LanguagePrimitives.GenericOne (max LanguagePrimitives.GenericZero v)

    let PositionScale (low:System.DateTimeOffset) (high:System.DateTimeOffset) (at:System.DateTimeOffset) =
        ClampScale <| (float (at.Ticks - low.Ticks))/(float (high.Ticks - low.Ticks))                   

    let InterpolatePosition (iv:FiniteIntervalValue<DateTimeOffset,'v>) p : float =
        PositionScale iv.start.position iv.``end``.position p

    let InterpolateValueLinear (iv:FiniteIntervalValue<'p,DateTimeOffset>) (scale:float) =
        let dv = iv.``end``.value.Subtract iv.start.value
        let f = TimeSpan.FromTicks <| int64 (float dv.Ticks * scale)
        iv.start.value + f

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
                      