module SSL_types :
  sig
    type ptvar = PVar of string
    type locvar = LVar of string
    type eq = Eqloc of locvar * locvar
    type affect = Pointsto of ptvar * locvar
    type affectnil = Pointsnil of ptvar
    type exists_loc = Exists of locvar
    type pure_formula = Pure of eq list * affect list * affectnil list
  end
