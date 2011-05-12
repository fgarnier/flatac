module SSL = struct 
  type ssl_ptvar = PVar of string
  type ssl_locvar = LVar of string
  
  type ssl_eq = Eqloc of LVar * LVar
  type ssl_affect = Pointsto of PVar * LVar
  type ssl_affectnil = Pointsnil of PVar
  type ssl_exists_loc = Exists of LVar

  type ssl_pure_formula = Pure of ssl_eq list * ssl_affect list * ssl_affectnil  
  

end;;
