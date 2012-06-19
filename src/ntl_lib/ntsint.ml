module P =
  struct
    type t = string
    type anot_type = unit
    let make_anot _ = ()
    let pprint_keyid s = s
    let pprint_anot _ = "" 
  end


module Nts_int = Nts_functor.Make(P)

