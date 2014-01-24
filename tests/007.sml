
datatype t = Var of Var.t
           | Param of Param.t
           | App of Func.t * t list
