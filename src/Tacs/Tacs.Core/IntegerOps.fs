namespace Tacs.Core

module IntegerOps =

    open Types

    let inline ClampScale (v:'a) =
        min LanguagePrimitives.GenericOne (max LanguagePrimitives.GenericZero v)

    let PositionScale (low:int) (high:int) (at:int) =
                ClampScale <| (float (at - low))/(float (high - low))                  

    let InterpolatePosition (iv:FiniteIntervalValue<int,'v>) p : float =
        PositionScale iv.start.position iv.``end``.position p

    let InterpolateValueLinear (iv:FiniteIntervalValue<'p,int>) (scale:float) =
        let dv = iv.``end``.value-iv.start.value
        let f = int (float dv * scale)
        iv.start.value + f

    let Interpolate (strat:InterpolationStrategy) (pScaler) (iv:FiniteIntervalValue<'p,int>) (pos:'p) : PointValue<'p,int> =
        let interpLin (v:FiniteIntervalValue<'p,int>) p : PointValue<'p,int> =
            let dv = v.``end``.value-v.start.value
            let s = pScaler v.start.position v.``end``.position p
            let f = int (float dv * s)
            {position=pos;value=(v.start.value + f)}

        let applyStrategy s (v:FiniteIntervalValue<'p,int>) (p:'p) =
            let vi = 
                match s with
                | Step -> v.start
                | Linear -> interpLin v p
            vi

        applyStrategy strat iv pos

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
                      