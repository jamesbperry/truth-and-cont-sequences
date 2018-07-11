namespace Tacs.Core

module FloatOps =

    open Types

    let inline ClampScale (v:'a) =
        min LanguagePrimitives.GenericOne (max LanguagePrimitives.GenericZero v)

    let FloatPosition (low:float) (high:float) (at:float) =
        ClampScale <| (at - low)/(high - low)           

    let InterpolatePosition (iv:Interval<float,'v>) p : float =
        FloatPosition iv.startbound.position iv.endbound.position p

    let InterpolateValueConstant (v:float) (p:'p) =
        v
    let InterpolateValueLinear<'p> (pinterp:PositionNormalizer<'p>) (pts:PointValue<'p,float>*PointValue<'p,float>) (p:'p) =
        let (pti, ptf) = pts
        let dv = ptf.value - pti.value
        let npos = pinterp pti.position ptf.position p
        pti.value + (dv * npos)

    // let Constant<'p> (startpos:IntervalBoundary<'p>) (endpos:IntervalBoundary<'p>) value = //refactor to dedupe
    //     let interp = InterpolateValueConstant value
    //     FiniteInterval {start=startpos;endbound=endpos;value=interp};

    type LinearFloatValue<'p> = 
        {pstart:PointValue<'p,float>;pend:PointValue<'p,float>} with
        interface IIntervalValue<'p,float> with
            member this.At pn p = InterpolateValueLinear pn (this.pstart, this.pend) p
            member this.Split pn p =
                let vmid = (this :> IIntervalValue<'p,float>).At pn p
                let pmid = {position=p;value=vmid}
                ({this with pend=pmid} :> IIntervalValue<'p,float>,{this with pstart=pmid} :> IIntervalValue<'p,float>)

    let LinearFloatValue (pstart,pend) =
        {pstart=pstart;pend=pend} :> IIntervalValue<'p,float>     

    let LinearFloatInterval (startb:BoundaryValue<'p,float>,endb:BoundaryValue<'p,float>) =
        {startbound=startb.position;endbound=endb.position;value={LinearFloatValue.pstart=PointValue.OfBoundary startb;pend=PointValue.OfBoundary endb}}      

    // let Linear<'p> (pinterp:PositionNormalizer<'p>) (startpt:BoundaryValue<'p,float>) (endpt:BoundaryValue<'p,float>) =
    //     let interp = InterpolateValueLinear pinterp (PointValue.ofBoundary startpt, PointValue.ofBoundary endpt)
    //     FiniteInterval {start=startpt.position;endbound=endpt.position;value=interp};

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
                      
                      