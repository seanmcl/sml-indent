
datatype t =
   L of { indent : int ref,
          origIndent : int,
          lineNum : int,
          contents : string,
          syms : Sym.t list,
          isComment : bool ref }
 | T of int
