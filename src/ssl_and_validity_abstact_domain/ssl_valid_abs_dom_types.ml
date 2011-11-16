open Var_validity_types
open Var_validity
open Ssl_types
open SSL_lex
open Composite_type_types


type ssl_validity_absdom = { ssl_part : ssl_formula ;
			     validinfos : validity_loc_map ;
			     composite_types_infos :  index_of_composite_types ;
			   }




