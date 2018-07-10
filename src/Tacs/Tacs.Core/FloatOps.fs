namespace Tacs.Core

module FloatOps =

    open Types

    let inline ClampScale (v:'a) =
        min LanguagePrimitives.GenericOne (max LanguagePrimitives.GenericZero v)

    let FloatPosition (low:float) (high:float) (at:float) =
        ClampScale <| (at - low)/(high - low)           

    let InterpolatePosition (iv:FiniteInterval<float,'v>) p : float =
        FloatPosition iv.start.position iv.``end``.position p

    let InterpolateValueConstant (v:float) (p:'p) =
        v
    let InterpolateValueLinear<'p> (pinterp:PositionNormalizer<'p>) (pts:PointValue<'p,float>*PointValue<'p,float>) (p:'p) =
        let (pti, ptf) = pts
        let dv = ptf.value - pti.value
        let npos = pinterp pti.position ptf.position p
        pti.value + (dv * npos)

    // let Constant<'p> (startpos:IntervalBoundary<'p>) (endpos:IntervalBoundary<'p>) value = //refactor to dedupe
    //     let interp = InterpolateValueConstant value
    //     FiniteInterval {start=startpos;``end``=endpos;value=interp};

    type LinearFloatValue<'p> = 
        {pstart:PointValue<'p,float>;pend:PointValue<'p,float>} with
        interface IIntervalValue<'p,float> with
            member this.At pn p = InterpolateValueLinear pn (this.pstart, this.pend) p

    let LinearFloatValue (pstart,pend) =
        {pstart=pstart;pend=pend} :> IIntervalValue<'p,float>     

    let LinearFloatInterval (startb:BoundaryValue<'p,float>,endb:BoundaryValue<'p,float>) =
        FiniteInterval {start=startb.position;``end``=endb.position;value={LinearFloatValue.pstart=PointValue.ofBoundary startb;pend=PointValue.ofBoundary endb}}      

    // let Linear<'p> (pinterp:PositionNormalizer<'p>) (startpt:BoundaryValue<'p,float>) (endpt:BoundaryValue<'p,float>) =
    //     let interp = InterpolateValueLinear pinterp (PointValue.ofBoundary startpt, PointValue.ofBoundary endpt)
    //     FiniteInterval {start=startpt.position;``end``=endpt.position;value=interp};

    let Integral (inseq:Interval<'a,float> seq) : (Interval<'a,float>) =
        failwith "not implemented"
    
    let Average (inseq:Interval<'a,float> seq) : (Interval<'a,float>) =
        failwith "not implemented"

    let Maximum (inseq:Interval<'a,float> seq) : (Interval<'a,float>) =
        failwith "not implemented"

    let Minimum (inseq:Interval<'a,float> seq) : (Interval<'a,float>) =
        failwith "not implemented"

    let Stdev (inseq:Interval<'a,float> seq) : (Interval<'a,float>) =
        failwith "not implemented"

    let ValueRange (inseq:Interval<'a,float> seq) : (Interval<'a,float>) =
        failwith "not implemented"
                      
                      