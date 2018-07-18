namespace Tacs.Core

module Piecewise =

    open System
    open Types

    type PiecewiseValue<'p,'iv> =
        {segments:'iv list} with
        interface IIntervalValue<'p,float> with
            member this.At _ _ = failwith "todo"
            member this.Split _ _ _ = failwith "todo"