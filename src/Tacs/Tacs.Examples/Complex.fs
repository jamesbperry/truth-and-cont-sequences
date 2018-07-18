namespace Tacs.Examples

module ComplexOps =

    open System
    open System.Numerics
    open Tacs.Core.Types
    open Tacs.Core.FloatOps

    let InterpolateValuePolar<'p> (pinterp:PositionNormalizer<'p>) (pts:PointValue<'p,Complex>*PointValue<'p,Complex>) (p:'p) =
        let (s, e) = pts
        let asPolar pt = (PointValue<_,_>.map (fun (v:Complex) -> v.Magnitude) pt, PointValue<_,_>.map (fun (v:Complex) -> v.Phase) pt)
        let (mags, phs) = asPolar s
        let (mage, phe) = asPolar e
        let magi= InterpolateValueLinear pinterp (mags,mage) p
        let phi = InterpolateValueLinear pinterp (phs,phe) p //TODO wrap
        Complex.FromPolarCoordinates (magi,phi)

    let InterpolateValueCartesian<'p> (pinterp:PositionNormalizer<'p>) (pts:PointValue<'p,Complex>*PointValue<'p,Complex>) (p:'p) =
        let (pti, ptf) = pts
        let asCartesian pt = (PointValue<_,_>.map (fun (v:Complex) -> v.Magnitude) pt, PointValue<_,_>.map (fun (v:Complex) -> v.Phase) pt)
        let (reali, imagi) = asCartesian pti
        let (realf, imagf) = asCartesian ptf
        let realinterp = InterpolateValueLinear pinterp (reali,realf) p
        let imaginterp = InterpolateValueLinear pinterp (imagi,imagf) p
        Complex (realinterp,imaginterp)

    type IComplexValue<'p> =
        inherit IIntervalValue<'p,Complex>
        //abstract member MyTypeSpecificAgg: Complex -> float

    type LinearPolarComplexValue<'p> = 
        {pstart:PointValue<'p,Complex>;pend:PointValue<'p,Complex>} with
        interface IIntervalValue<'p,Complex> with
            member this.At pn p = InterpolateValuePolar pn (this.pstart, this.pend) p
            member this.Split pn p self =
                if not <| Object.ReferenceEquals (self,this) then invalidArg "self" "Pass the object itself as the third argument to its own Split() function. Yes, this is weird."
                let vmid = (this :> IIntervalValue<'p,Complex>).At pn p
                let pmid = {position=p;value=vmid}
                (asi {this with pend=pmid},asi {this with pstart=pmid})
        interface IComplexValue<'p> end

    let LinearPolarComplexInterval (startb:BoundaryValue<'p,Complex>,endb:BoundaryValue<'p,Complex>) =
        {startbound=startb.position;endbound=endb.position;value={LinearPolarComplexValue.pstart=PointValue.ofBoundary startb;pend=PointValue.ofBoundary endb} :> IComplexValue<_>}      

    type LinearCartesianComplexValue<'p> = 
        {pstart:PointValue<'p,Complex>;pend:PointValue<'p,Complex>} with
        interface IIntervalValue<'p,Complex> with
            member this.At pn p = InterpolateValueCartesian pn (this.pstart, this.pend) p
            member this.Split pn p self =
                if not <| Object.ReferenceEquals (self,this) then invalidArg "self" "Pass the object itself as the third argument to its own Split() function. Yes, this is weird."
                let vmid = (this :> IIntervalValue<'p,Complex>).At pn p
                let pmid = {position=p;value=vmid}
                (asi {this with pend=pmid},asi {this with pstart=pmid})
        interface IComplexValue<'p> end

    let LinearCartesianComplexInterval (startb:BoundaryValue<'p,Complex>,endb:BoundaryValue<'p,Complex>) =
        {startbound=startb.position;endbound=endb.position;value={LinearCartesianComplexValue.pstart=PointValue.ofBoundary startb;pend=PointValue.ofBoundary endb} :> IComplexValue<_>}      
