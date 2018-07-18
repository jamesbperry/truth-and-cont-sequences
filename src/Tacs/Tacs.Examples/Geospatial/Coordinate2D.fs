namespace Tacs.Examples.Geospatial

module Coordinate2D =

    open System
    open Tacs.Core.Types
    open Tacs.Core.FloatOps

    type T = {x:float;y:float} with//oversimplified for now
        static member X p = p.x
        static member Y p = p.y 

    let InterpolateValueLinear<'p> (pinterp:PositionNormalizer<'p>) (pti:PointValue<'p,T>,ptf:PointValue<'p,T>) (p:'p) =
        let getx = PointValue.map T.X
        let gety = PointValue.map T.Y
        let xinterp = InterpolateValueLinear pinterp (getx pti,getx ptf) p
        let yinterp = InterpolateValueLinear pinterp (gety pti, gety ptf) p
        {x=xinterp;y=yinterp}

    type ICoordinate2DValue<'p> =
        inherit IIntervalValue<'p,T>
        //abstract member MyTypeSpecificAgg: Point -> float

    type LinearCoordinate2DValue<'p> = 
        {pstart:PointValue<'p,T>;pend:PointValue<'p,T>} with
        interface IIntervalValue<'p,T> with
            member this.At pn p = InterpolateValueLinear pn (this.pstart, this.pend) p
            member this.Split pn p self =
                if not <| Object.ReferenceEquals (self,this) then invalidArg "self" "Pass the object itself as the third argument to its own Split() function. Yes, this is weird."
                let vmid = (this :> IIntervalValue<'p,T>).At pn p
                let pmid = {position=p;value=vmid}
                (asi {this with pend=pmid},asi {this with pstart=pmid})
        interface ICoordinate2DValue<'p> end

    let LinearCoordinate2DValue (pstart,pend) = {LinearCoordinate2DValue.pstart=pstart;pend=pend} :> ICoordinate2DValue<_>

    let LinearCoordinate2DInterval (startb:BoundaryValue<'p,T>,endb:BoundaryValue<'p,T>) =
        {startbound=startb.position;endbound=endb.position;value={LinearCoordinate2DValue.pstart=PointValue.ofBoundary startb;pend=PointValue.ofBoundary endb} :> ICoordinate2DValue<_>}      
