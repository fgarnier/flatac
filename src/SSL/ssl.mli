open 

module SSL :
  sig
    val create : unit -> ssl_formula
    val normalize : ssl_formula -> ssl_formula
    val garbage : ssl_formula -> bool
    val biabduction : ssl_formula -> ssl_formula -> ( ssl_formula * ssl_formula)
      
  end;;
