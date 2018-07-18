namespace Tacs.Examples.Geospatial

module Envelope2D =

    open System
    open Tacs.Core.Types

    type T = {top:float;bottom:float;left:float;right:float} with //oversimplified for now
        static member BottomLeft env : Coordinate2D.T = {x=env.left;y=env.bottom}
        static member TopRight env : Coordinate2D.T = {x=env.right;y=env.top}
        static member Centroid env : Coordinate2D.T = {x=(env.left+env.right)/2.0;y=(env.top+env.bottom)/2.0}

    let InterpolateValueLinear<'p> (pinterp:PositionNormalizer<'p>) (pti:PointValue<'p,T>,ptf:PointValue<'p,T>) (p:'p) =
        let getbl = PointValue.map T.BottomLeft
        let gettr = PointValue.map T.TopRight
        let blinterp = Coordinate2D.InterpolateValueLinear pinterp (getbl pti,getbl ptf) p
        let trinterp = Coordinate2D.InterpolateValueLinear pinterp (gettr pti, gettr ptf) p
        {top=trinterp.y;bottom=blinterp.y;left=blinterp.x;right=trinterp.y}

    type IEnvelope2DValue<'p> =
        inherit IIntervalValue<'p,T>
        abstract member Centroid: Coordinate2D.ICoordinate2DValue<'p>

    type LinearEnvelope2DValue<'p> = 
        {pstart:PointValue<'p,T>;pend:PointValue<'p,T>} with
        interface IIntervalValue<'p,T> with
            member this.At pn p = InterpolateValueLinear pn (this.pstart, this.pend) p
            member this.Split pn p self =
                if not <| Object.ReferenceEquals (self,this) then invalidArg "self" "Pass the object itself as the third argument to its own Split() function. Yes, this is weird."
                let vmid = (this :> IIntervalValue<'p,T>).At pn p
                let pmid = {position=p;value=vmid}
                (asi {this with pend=pmid},asi {this with pstart=pmid})
        interface IEnvelope2DValue<'p> with
            member this.Centroid =
                let getcp (p:PointValue<_,_>) =
                     let c = T.Centroid p.value
                     {position=p.position;value=c}
                let bounds = (getcp this.pstart,getcp this.pend)
                Coordinate2D.LinearCoordinate2DValue bounds

    let LinearEnvelope2DInterval (startb:BoundaryValue<'p,T>,endb:BoundaryValue<'p,T>) =
        {startbound=startb.position;endbound=endb.position;value={LinearEnvelope2DValue.pstart=PointValue.ofBoundary startb;pend=PointValue.ofBoundary endb} :> IEnvelope2DValue<_>}      
