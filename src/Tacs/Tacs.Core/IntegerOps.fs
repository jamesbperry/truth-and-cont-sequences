namespace Tacs.Core

module IntegerOps =

    open Types

    let inline ClampScale (v:'a) =
        min LanguagePrimitives.GenericOne (max LanguagePrimitives.GenericZero v)

    let PositionScale (low:int) (high:int) (at:int) =
                ClampScale <| (float (at - low))/(float (high - low))                  

    let InterpolatePosition (iv:FiniteInterval<int,'v>) p : float =
        PositionScale iv.start.position iv.``end``.position p

    let InterpolateValueConstant (v:int) (p:'p) : int =
        v

    let InterpolateValueLinearNearest<'p> (pinterp:PositionNormalizer<'p>) (pts:PointValue<'p,int>*PointValue<'p,int>) (p:'p) =
        let (pti, ptf) = pts
        let dv = ptf.value - pti.value
        let npos = pinterp pti.position ptf.position p
        let f = int (float dv * npos)
        pti.value + f

    let InterpolateValueLinearFloor<'p> (pinterp:PositionNormalizer<'p>) (pts:PointValue<'p,int>*PointValue<'p,int>) (p:'p) = 
        let (pti, ptf) = pts
        let dv = ptf.value - pti.value
        let npos = pinterp pti.position ptf.position p
        let f = int <| floor (float dv * npos)
        pti.value + f

    let InterpolateValueLinearCeiling<'p> (pinterp:PositionNormalizer<'p>) (pts:PointValue<'p,int>*PointValue<'p,int>) (p:'p) = 
        let (pti, ptf) = pts
        let dv = ptf.value - pti.value
        let npos = pinterp pti.position ptf.position p
        let f = int <| ceil (float dv * npos)
        pti.value + f

    type LinearNearestIntValue<'p> = 
        {pstart:PointValue<'p,int>;pend:PointValue<'p,int>} with
        interface IIntervalValue<'p,int> with
            member this.At pn p = InterpolateValueLinearNearest pn (this.pstart, this.pend) p

    type LinearFloorIntValue<'p> = 
        {pstart:PointValue<'p,int>;pend:PointValue<'p,int>} with
        interface IIntervalValue<'p,int> with
            member this.At pn p = InterpolateValueLinearFloor pn (this.pstart, this.pend) p

    type LinearCeilingIntValue<'p> = 
        {pstart:PointValue<'p,int>;pend:PointValue<'p,int>} with
        interface IIntervalValue<'p,int> with
            member this.At pn p = InterpolateValueLinearCeiling pn (this.pstart, this.pend) p

    let LinearNearestIntValue (pstart,pend) =
        {LinearNearestIntValue.pstart=pstart;pend=pend} :> IIntervalValue<'p,int>  

    let LinearFloorIntValue (pstart,pend) =
        {LinearCeilingIntValue.pstart=pstart;pend=pend} :> IIntervalValue<'p,int>    

    let LinearCeilingIntValue (pstart,pend) =
        {LinearCeilingIntValue.pstart=pstart;pend=pend} :> IIntervalValue<'p,int>                     

    // let Constant<'p> (startpos:IntervalBoundary<'p>) (endpos:IntervalBoundary<'p>) value = //refactor to dedupe
    //     let interp = InterpolateValueConstant value
    //     FiniteInterval {start=startpos;``end``=endpos;value=interp};

    // let LinearNearest<'p> (pinterp:PositionNormalizer<'p>) (startpt:BoundaryValue<'p,int>) (endpt:BoundaryValue<'p,int>) = //refactor to dedupe
    //     let interp = InterpolateValueLinearNearest pinterp (PointValue.ofBoundary startpt, PointValue.ofBoundary endpt)
    //     FiniteInterval {start=startpt.position;``end``=endpt.position;value=interp};

    // let LinearFloor<'p> (pinterp:PositionNormalizer<'p>) (startpt:BoundaryValue<'p,int>) (endpt:BoundaryValue<'p,int>) =
    //     let interp = InterpolateValueLinearFloor pinterp (PointValue.ofBoundary startpt, PointValue.ofBoundary endpt)
    //     FiniteInterval {start=startpt.position;``end``=endpt.position;value=interp};

    // let LinearCeiling<'p> (pinterp:PositionNormalizer<'p>) (startpt:BoundaryValue<'p,int>) (endpt:BoundaryValue<'p,int>) =
    //     let interp = InterpolateValueLinearCeiling pinterp (PointValue.ofBoundary startpt, PointValue.ofBoundary endpt)
    //     FiniteInterval {start=startpt.position;``end``=endpt.position;value=interp};

    //Integer
    let Integral (inseq:Interval<'a,int> seq) : (Interval<'a,int>) =
        failwith "not implemented"
    
    let Average (inseq:Interval<'a,int> seq) : (Interval<'a,int>) =
        failwith "not implemented"

    let Maximum (inseq:Interval<'a,int> seq) : (Interval<'a,int>) =
        failwith "not implemented"

    let Minimum (inseq:Interval<'a,int> seq) : (Interval<'a,int>) =
        failwith "not implemented"

    let Stdev (inseq:Interval<'a,int> seq) : (Interval<'a,int>) =
        failwith "not implemented"

    let ValueRange (inseq:Interval<'a,int> seq) : (Interval<'a,int>) =
        failwith "not implemented"
                      