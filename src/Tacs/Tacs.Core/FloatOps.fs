namespace Tacs.Core

module FloatOps =

    open Types

    let inline ClampScale (v:'a) =
        min LanguagePrimitives.GenericOne (max LanguagePrimitives.GenericZero v)

    let PositionScale (low:float) (high:float) (at:float) =
        ClampScale <| (at - low)/(high - low)           

    let InterpolatePosition (iv:FiniteIntervalValue<float,'v>) p : float =
        PositionScale iv.start.position iv.``end``.position p

    let InterpolateValueConstant (v:float) (p:'p) =
        v
    let InterpolateValueLinear<'p> (pinterp:PositionNormalizer<'p>) (pts:PointValue<'p,float>*PointValue<'p,float>) (p:'p) =
        let (pti, ptf) = pts
        let dv = ptf.value - pti.value
        let npos = pinterp pti.position ptf.position p
        pti.value + (dv * npos)

    let Constant<'p> (startpos:IntervalBoundary<'p>) (endpos:IntervalBoundary<'p>) value = //refactor to dedupe
        let interp = InterpolateValueConstant value
        FiniteIntervalValue {start=startpos;``end``=endpos;value=interp};

    let Linear<'p> (pinterp:PositionNormalizer<'p>) (startpt:BoundaryValue<'p,float>) (endpt:BoundaryValue<'p,float>) =
        let interp = InterpolateValueLinear pinterp (PointValue.ofBoundary startpt, PointValue.ofBoundary endpt)
        FiniteIntervalValue {start=startpt.position;``end``=endpt.position;value=interp};

    let Integral (inseq:IntervalValue<'a,float> seq) : (IntervalValue<'a,float>) =
        failwith "not implemented"
    
    let Average (inseq:IntervalValue<'a,float> seq) : (IntervalValue<'a,float>) =
        failwith "not implemented"

    let Maximum (inseq:IntervalValue<'a,float> seq) : (IntervalValue<'a,float>) =
        failwith "not implemented"

    let Minimum (inseq:IntervalValue<'a,float> seq) : (IntervalValue<'a,float>) =
        failwith "not implemented"

    let Stdev (inseq:IntervalValue<'a,float> seq) : (IntervalValue<'a,float>) =
        failwith "not implemented"

    let ValueRange (inseq:IntervalValue<'a,float> seq) : (IntervalValue<'a,float>) =
        failwith "not implemented"
                      
                      