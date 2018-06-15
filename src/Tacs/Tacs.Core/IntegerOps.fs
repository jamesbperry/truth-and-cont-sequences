namespace Tacs.Core

module IntegerOps =

    open Types

    let inline ClampScale (v:'a) =
        min LanguagePrimitives.GenericOne (max LanguagePrimitives.GenericZero v)

    let PositionScale (low:int) (high:int) (at:int) =
                ClampScale <| (float (at - low))/(float (high - low))                  

    let InterpolatePosition (iv:FiniteIntervalValue<int,'v>) p : float =
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

    let Constant<'p> (startpos:IntervalBoundary<'p>) (endpos:IntervalBoundary<'p>) value = //refactor to dedupe
        let interp = InterpolateValueConstant value
        FiniteIntervalValue {start=startpos;``end``=endpos;value=interp};

    let LinearNearest<'p> (pinterp:PositionNormalizer<'p>) (startpt:BoundaryValue<'p,int>) (endpt:BoundaryValue<'p,int>) = //refactor to dedupe
        let interp = InterpolateValueLinearNearest pinterp (PointValue.ofBoundary startpt, PointValue.ofBoundary endpt)
        FiniteIntervalValue {start=startpt.position;``end``=endpt.position;value=interp};

    let LinearFloor<'p> (pinterp:PositionNormalizer<'p>) (startpt:BoundaryValue<'p,int>) (endpt:BoundaryValue<'p,int>) =
        let interp = InterpolateValueLinearFloor pinterp (PointValue.ofBoundary startpt, PointValue.ofBoundary endpt)
        FiniteIntervalValue {start=startpt.position;``end``=endpt.position;value=interp};

    let LinearCeiling<'p> (pinterp:PositionNormalizer<'p>) (startpt:BoundaryValue<'p,int>) (endpt:BoundaryValue<'p,int>) =
        let interp = InterpolateValueLinearCeiling pinterp (PointValue.ofBoundary startpt, PointValue.ofBoundary endpt)
        FiniteIntervalValue {start=startpt.position;``end``=endpt.position;value=interp};

    //Integer
    let Integral (inseq:IntervalValue<'a,int> seq) : (IntervalValue<'a,int>) =
        failwith "not implemented"
    
    let Average (inseq:IntervalValue<'a,int> seq) : (IntervalValue<'a,int>) =
        failwith "not implemented"

    let Maximum (inseq:IntervalValue<'a,int> seq) : (IntervalValue<'a,int>) =
        failwith "not implemented"

    let Minimum (inseq:IntervalValue<'a,int> seq) : (IntervalValue<'a,int>) =
        failwith "not implemented"

    let Stdev (inseq:IntervalValue<'a,int> seq) : (IntervalValue<'a,int>) =
        failwith "not implemented"

    let ValueRange (inseq:IntervalValue<'a,int> seq) : (IntervalValue<'a,int>) =
        failwith "not implemented"
                      