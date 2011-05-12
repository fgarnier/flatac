

module SSL_types :
  sig
    type ptvar = PVar of string
    type locvar = LVar of string
    type eq = Eqloc of locvar * locvar
    type affect = Pointsto of ptvar * locvar
    type affectnil = Pointsnil of ptvar
    type exists_loc = Exists of locvar
    type pure_formula = Pure of eq list * affect list * affectnil list
    
    type heap_atom = Alloc of locvar
		     | Emp

    type space_formula = Space of heap_atom list
    type heap = 
	
    val normalize :  
  end
