namespace Tacs.Examples

module SetOps =

    open Tacs.Core.Types

    type ISetValue<'p,'key when 'key : comparison> =
        inherit IIntervalValue<'p,Set<'key>>
        //abstract member Foo : Bar

    type ConstantSetValue<'p,'key when 'key : comparison> = 
        {members:Set<'key>} with
        interface IIntervalValue<'p,Set<'key>> with
            member this.At _ _ = this.members
            member this.Split _ _ _ = (asi this,asi this)
        interface ISetValue<'p,'key> 
        end

    