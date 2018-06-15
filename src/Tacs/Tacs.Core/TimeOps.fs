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

    let InterpolateValueConstant (v:DateTimeOffset) (p:'p)  =
        v
        
    let InterpolateValueLinear<'p> (pinterp:PositionNormalizer<'p>) (pts:PointValue<'p,DateTimeOffset>*PointValue<'p,DateTimeOffset>) (p:'p) =
        let (pti, ptf) = pts
        let dv = ptf.value.Subtract pti.value
        let npos = pinterp pti.position ptf.position p
        let del = TimeSpan.FromTicks <| int64 (float dv.Ticks * npos)
        pti.value + del

    let Constant<'p> (startpos:IntervalBoundary<'p>) (endpos:IntervalBoundary<'p>) value = //refactor to dedupe
        let interp = InterpolateValueConstant value
        FiniteIntervalValue {start=startpos;``end``=endpos;value=interp};

    let Linear<'p> (pinterp:PositionNormalizer<'p>) (startpt:BoundaryValue<'p,DateTimeOffset>) (endpt:BoundaryValue<'p,DateTimeOffset>) =
        let interp = InterpolateValueLinear pinterp (PointValue.ofBoundary startpt, PointValue.ofBoundary endpt)
        FiniteIntervalValue {start=startpt.position;``end``=endpt.position;value=interp};

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
                      