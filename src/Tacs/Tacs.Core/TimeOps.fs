namespace Tacs.Core

module TimeOps =

    open Types
    open System

    let inline ClampScale (v:'a) =
        min LanguagePrimitives.GenericOne (max LanguagePrimitives.GenericZero v)

    let TimePosition (low:System.DateTimeOffset) (high:System.DateTimeOffset) (at:System.DateTimeOffset) =
        ClampScale <| (float (at.Ticks - low.Ticks))/(float (high.Ticks - low.Ticks))                   

    let InterpolatePosition (iv:FiniteInterval<DateTimeOffset,'v>) p : float =
        TimePosition iv.start.position iv.``end``.position p

    let InterpolateValueConstant (v:DateTimeOffset) (p:'p)  =
        v
        
    let InterpolateValueLinear<'p> (pinterp:PositionNormalizer<'p>) (pts:PointValue<'p,DateTimeOffset>*PointValue<'p,DateTimeOffset>) (p:'p) =
        let (pti, ptf) = pts
        let dv = ptf.value.Subtract pti.value
        let npos = pinterp pti.position ptf.position p
        let del = TimeSpan.FromTicks <| int64 (float dv.Ticks * npos)
        pti.value + del

    // let Constant<'p> (startpos:IntervalBoundary<'p>) (endpos:IntervalBoundary<'p>) value = //refactor to dedupe
    //     let interp = InterpolateValueConstant value
    //     FiniteInterval {start=startpos;``end``=endpos;value=interp};

    // let Linear<'p> (pinterp:PositionNormalizer<'p>) (startpt:BoundaryValue<'p,DateTimeOffset>) (endpt:BoundaryValue<'p,DateTimeOffset>) =
    //     let interp = InterpolateValueLinear pinterp (PointValue.ofBoundary startpt, PointValue.ofBoundary endpt)
    //     FiniteInterval {start=startpt.position;``end``=endpt.position;value=interp};

    type LinearTimeValue<'p> = 
        {pstart:PointValue<'p,DateTimeOffset>;pend:PointValue<'p,DateTimeOffset>} with
        interface IIntervalValue<'p,DateTimeOffset> with
            member this.At pn p = InterpolateValueLinear pn (this.pstart, this.pend) p

    let LinearTimeValue (pstart,pend) =
        {LinearTimeValue.pstart=pstart;pend=pend} :> IIntervalValue<'p,DateTimeOffset>  

    let LinearTimeInterval (startb:BoundaryValue<'p,DateTimeOffset>,endb:BoundaryValue<'p,DateTimeOffset>) =
        FiniteInterval {start=startb.position;``end``=endb.position;value={LinearTimeValue.pstart=PointValue.ofBoundary startb;pend=PointValue.ofBoundary endb}} 

    //Integer
    let Integrate (inseq:Interval<'a,DateTimeOffset> seq) : (Interval<'a,DateTimeOffset>) =
        failwith "not implemented"
    
    let Average (inseq:Interval<'a,DateTimeOffset> seq) : (Interval<'a,DateTimeOffset>) =
        failwith "not implemented"

    let Maximum (inseq:Interval<'a,DateTimeOffset> seq) : (Interval<'a,DateTimeOffset>) =
        failwith "not implemented"

    let Minimum (inseq:Interval<'a,DateTimeOffset> seq) : (Interval<'a,DateTimeOffset>) =
        failwith "not implemented"

    let Stdev (inseq:Interval<'a,DateTimeOffset> seq) : (Interval<'a,DateTimeOffset>) =
        failwith "not implemented"

    let ValueRange (inseq:Interval<'a,DateTimeOffset> seq) : (Interval<'a,DateTimeOffset>) =
        failwith "not implemented"
                      