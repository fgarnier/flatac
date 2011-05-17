
module type SSL : functor ( P : sig
			      val order_relation : string -> string -> bool 
			    end
			  )
  sig
    val create : unit -> ssl_formula
    val normalize : ssl_formula -> ssl_formula
    val garbage : ssl_formula -> bool
    val biabduction : ssl_formula -> ssl_formula -> ( ssl_formula * ssl_formula)
    val is_error_state : ssl
      
  end;;
