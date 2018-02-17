namespace Tacs.Core

module FloatOps =

    open Types

    let inline ClampScale (v:'a) =
        min LanguagePrimitives.GenericOne (max LanguagePrimitives.GenericZero v)

    let PositionScale (low:float) (high:float) (at:float) =
        ClampScale <| (at - low)/(high - low)           

    let InterpolatePosition (iv:FiniteIntervalValue<float,'v>) p : float =
        PositionScale iv.start.position iv.``end``.position p

    let InterpolateValueLinear (iv:FiniteIntervalValue<'p,float>) (scale:float) =
        let dv = iv.``end``.value-iv.start.value
        let f = (dv * scale)
        iv.start.value + f

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
                      
                      