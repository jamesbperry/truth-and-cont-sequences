namespace Tacs.Core

 module Numerics =

    open Types
    open System

    type FloatPosition() =
        interface ISequencePositionType<float> with
            // member this.Diff high low =
            //     high - low
            // member this.Add p dp =
            //     p + dp       
            // member this.Div n d =
            //     n / d       
            member this.Scale low high at =
                (at - low)/(high - low)                 

    type IntPosition() =            
        interface ISequencePositionType<int> with
            // member this.Diff high low =
            //     high - low
            // member this.Add p dp =
            //     p + dp       
            // member this.Div n d =
            //     (float n) / (float d)     
            member this.Scale low high at =
                (float (at - low))/(float (high - low))                  

    type TimePosition() =
        interface ISequencePositionType<DateTimeOffset> with
            // member this.Diff high low =
            //     high - low        
            // member this.Add p dp =
            //     p + dp                       
            // member this.Div n d =
            //     (float n.Ticks) / (float d.Ticks)
            member this.Scale low high at =
                (float (at.Ticks - low.Ticks))/(float (high.Ticks - low.Ticks))                   

    type FloatValueStrategy<'p,'dp> (strat:InterpolationStrategy,pt:ISequencePositionType<'p>) =
        interface ISequenceValueStrategy<'p,float> with
            member this.Strategy = strat
            member this.Interpolate (iv:FiniteIntervalValue<'p,float>) (pos:'p) : PointValue<'p,float> =
                let interpLin (v:FiniteIntervalValue<'p,float>) p : PointValue<'p,float> =
                    let dv = v.``end``.value-v.start.value
                    let s = pt.Scale iv.start.position iv.``end``.position pos
                    let f = (dv * s)
                    {position=pos;value=(v.start.value + f)}

                let applyStrategy s (v:FiniteIntervalValue<'p,float>) (p:'p) =
                    let vi = 
                        match s with
                        | Step -> v.start
                        | Linear -> interpLin v p
                    vi

                applyStrategy strat iv pos          
    type IntValueStrategy<'p,'dp> (strat:InterpolationStrategy,pt:ISequencePositionType<'p>) =
        interface ISequenceValueStrategy<'p,int> with
            member this.Strategy = strat
            member this.Interpolate (iv:FiniteIntervalValue<'p,int>) (pos:'p) : PointValue<'p,int> =
                let interpLin (v:FiniteIntervalValue<'p,int>) p : PointValue<'p,int> =
                    let dv = v.``end``.value-v.start.value
                    let s = pt.Scale iv.start.position iv.``end``.position pos
                    let f = int (float dv * s)
                    {position=pos;value=(v.start.value + f)}

                let applyStrategy s (v:FiniteIntervalValue<'p,int>) (p:'p) =
                    let vi = 
                        match s with
                        | Step -> v.start
                        | Linear -> interpLin v p
                    vi

                applyStrategy strat iv pos                  