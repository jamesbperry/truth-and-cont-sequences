namespace Tacs.Examples

module ElapsedTimeOps =

    open System
    open Tacs.Core.Types

    let inline ClampScale (v:'a) = //TODO make common
        min LanguagePrimitives.GenericOne (max LanguagePrimitives.GenericZero v)

    let ElapsedTimePosition (low:System.TimeSpan) (high:System.TimeSpan) (at:System.TimeSpan) =
        ClampScale <| (float (at.Ticks - low.Ticks))/(float (high.Ticks - low.Ticks))                   

    let OnElapsedTimePosition (unitsize:float) (ints:Interval<TimeSpan,'v,'i> list) =
        let weight (int:Interval<TimeSpan,'v,'i>) u = float (int.endbound.position.Ticks - int.startbound.position.Ticks) / u
        List.map (fun (int:Interval<TimeSpan,'v,'i>) -> { interval=int;weight=weight int unitsize}) ints

    let InterpolatePosition (iv:Interval<TimeSpan,'v,_>) p : float =
        ElapsedTimePosition iv.startbound.position iv.endbound.position p