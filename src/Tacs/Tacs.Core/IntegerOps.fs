namespace Tacs.Core

module IntegerOps =

    open Types

    let inline ClampScale (v:'a) =
        min LanguagePrimitives.GenericOne (max LanguagePrimitives.GenericZero v)

    let PositionScale (low:int) (high:int) (at:int) =
                ClampScale <| (float (at - low))/(float (high - low))                  

    let InterpolatePosition (iv:FiniteIntervalValue<int,'v>) p : float =
        PositionScale iv.start iv.``end`` p

    let InterpolateValueConstant (v:int) (npos:NormalizedPosition) : int =
        v

    let InterpolateValueLinear<'p> (pinterp:PositionNormalizer<'p>) (pts:PointValue<'p,int>*PointValue<'p,int>) (p:'p) =
        let (pti, ptf) = pts
        let dv = ptf.value - pti.value
        let npos = pinterp pti.position ptf.position p
        let f = int (float dv * npos)
        pti.value + f

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
                      