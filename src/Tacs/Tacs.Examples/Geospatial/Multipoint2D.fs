namespace Tacs.Examples.Geospatial

module Multipoint2D =

    open System
    open Tacs.Core.Types

    type T = {coords:Coordinate2D.T list} // with //oversimplified for now

    type IMultipoint2DValue<'p> =
        inherit IIntervalValue<'p,T>
        abstract member Centroid: Coordinate2D.ICoordinate2DValue<'p>
        abstract member Perimeter: Tacs.Core.FloatOps.IFloatValue<'p>

    type ConstantMultipoint2DValue<'p> = 
        {constantvalue:T} with
        interface IIntervalValue<'p,T> with
            member this.At _ _ = this.constantvalue
            member this.Split _ _ _ = (asi this,asi this)
        interface IMultipoint2DValue<'p> with
            member this.Centroid = failwith "todo"
            member this.Perimeter = failwith "todo"