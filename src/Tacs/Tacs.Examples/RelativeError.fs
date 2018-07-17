namespace Tacs.Examples

//Example of a custom intervalvalue type to demonstrate relative error propagation
module RelativeError =

    open Tacs.Core.Types
    open Tacs.Core.FloatOps

    type RelativeErrorFloat = 
        {value:float;error:float} with
        static member (+) (iv1:RelativeErrorFloat,iv2:RelativeErrorFloat) : RelativeErrorFloat = 
            let v = iv1.value + iv2.value
            let enumer = iv1.error * iv1.value + iv2.error * iv2.value
            let err = enumer / (iv1.value + iv2.value)
            {value=v;error=err}

        static member (-) (iv1:RelativeErrorFloat,iv2:RelativeErrorFloat) : RelativeErrorFloat = 
            let v = iv1.value - iv2.value
            let enumer = iv1.error * iv1.value - iv2.error * iv2.value
            let err = enumer / (iv1.value - iv2.value)
            {value=v;error=err}

        static member (*) (iv1:RelativeErrorFloat,iv2:RelativeErrorFloat) : RelativeErrorFloat =
            let v = iv1.value * iv2.value
            let err = iv1.error + iv2.error + (iv1.error * iv2.error)
            {value=v;error=err}

        static member (/) (iv1:RelativeErrorFloat,iv2:RelativeErrorFloat) : RelativeErrorFloat =
            let v = iv1.value / iv2.value
            let err = iv1.error - iv2.error - (iv1.error * iv2.error)
            {value=v;error=err}

        static member (+) (iv1:RelativeErrorFloat,dv:float) : RelativeErrorFloat = 
            let v = iv1.value + dv
            {iv1 with value=v}

        static member (-) (iv1:RelativeErrorFloat,dv:float): RelativeErrorFloat = 
            let v = iv1.value - dv
            {iv1 with value=v}

        static member (*) (iv1:RelativeErrorFloat,v:float) : RelativeErrorFloat =
            let v = iv1.value * v
            let err = iv1.error * v
            {value=v;error=err}

        static member (/) (iv1:RelativeErrorFloat,v:float) =
            let v = iv1.value / v
            let err = iv1.error / v
            {value=v;error=err}

    type RelativeErrorFloatValue<'p> = 
        {value:IFloatValue<'p>;error:IFloatValue<'p>} with
        interface IIntervalValue<'p,RelativeErrorFloat> with
            member this.At pn p = {value=this.value.At pn p;error=this.error.At pn p}
            member this.Split pn p self =
                let (vbef,vaft) = this.value.Split pn p this.value
                let (ibef,iaft) = this.error.Split pn p this.error
                let bef = {value=vbef;error=ibef}
                let aft = {value=vaft;error=iaft}
                (asi <| bef,asi <| aft)

