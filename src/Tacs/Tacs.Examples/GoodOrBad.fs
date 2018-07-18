namespace Tacs.Examples

//Example of a custom intervalvalue type using a DU
module GoodOrBad =

    open Tacs.Core.Types

    type TPoint<'good,'bad> =
        | GoodPointValue of 'good 
        | BadPointValue of 'bad

    type GoodOrBadValue<'p,'g,'b> = 
        | GoodValue of IIntervalValue<'p,'g>
        | BadValue of IIntervalValue<'p,'b>
        interface IIntervalValue<'p,TPoint<'g,'b>> with
            member this.At pn p = 
                match this with
                | GoodValue gv -> GoodPointValue (gv.At pn p)
                | BadValue bv -> BadPointValue (bv.At pn p)
            member this.Split pn p self =
                match this with
                | GoodValue gv -> 
                    let (bef,aft) = (gv.Split pn p gv)
                    let asInt v = GoodValue v : GoodOrBadValue<'p,'g,'b>
                    (asi <| asInt bef, asi <| asInt aft)
                | BadValue bv ->  
                    let (bef,aft) = (bv.Split pn p bv)
                    let asInt v = BadValue v : GoodOrBadValue<'p,'g,'b>
                    (asi <| asInt bef, asi <| asInt aft)
