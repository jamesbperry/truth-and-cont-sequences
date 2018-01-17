namespace Tacs.Core

 module Numerics =

    open Types

    let inline ClampScale (v:'a) =
        min LanguagePrimitives.GenericOne (max LanguagePrimitives.GenericZero v)

    let FloatPositionScale (low:float) (high:float) (at:float) =
                ClampScale <| (at - low)/(high - low)           

    let IntPositionScale (low:int) (high:int) (at:int) =
                ClampScale <| (float (at - low))/(float (high - low))                  

    let TimePositionScale (low:System.DateTimeOffset) (high:System.DateTimeOffset) (at:System.DateTimeOffset) =
                ClampScale <| (float (at.Ticks - low.Ticks))/(float (high.Ticks - low.Ticks))                   

    let InterpolateFloat (strat:InterpolationStrategy) (pScaler) (iv:FiniteIntervalValue<'p,float>) (pos:'p) : PointValue<'p,float> =
        let interpLin (v:FiniteIntervalValue<'p,float>) p : PointValue<'p,float> =
            let dv = v.``end``.value-v.start.value
            let s = pScaler v.start.position v.``end``.position p
            let f = (dv * s)
            {position=pos;value=(v.start.value + f)}

        let applyStrategy s (v:FiniteIntervalValue<'p,float>) (p:'p) =
            let vi = 
                match s with
                | Step -> v.start
                | Linear -> interpLin v p
            vi

        applyStrategy strat iv pos          

    let InterpolateInt (strat:InterpolationStrategy) (pScaler) (iv:FiniteIntervalValue<'p,int>) (pos:'p) : PointValue<'p,int> =
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