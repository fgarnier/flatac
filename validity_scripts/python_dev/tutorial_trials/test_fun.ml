let rec eq_len_list = function
  | ( (a::l) , (b :: m) ) -> eq_len_list (l,m) 
  | ([],[]) -> true
  | (_,_) -> false
    
