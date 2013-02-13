(*This files contains the implementation of the Numerical Transition Library 



Written by Florent Garnier, at Verimag Labs  2012 
Contact florent dot garnier at gmail dot com for  further informations.

This files is released under the terms of the LGPL v2.1 Licence.

 
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor,
 Boston, MA  02110-1301  USA
*)

(**
Most basic operations over nts types definition.
*)

exception CntInvalidExpression
val nts_pprint_nts_var : Nts_types.nts_var -> string
val nts_get_nts_gen_var_name : Nts_types.nts_genrel_var -> string
val nts_pprint_btype : Nts_types.nts_base_types -> string
val nts_pprint_nts_typeinfo_var : Nts_types.nts_var -> string
val pprint_nts_var_list : Nts_types.nts_var list -> string
val pprint_nts_and_prime_var_list : Nts_types.nts_var list -> string
val concat_if_first_arg_nonzero : string -> string -> string
val concat_comma_both_arg_non_empty : string -> string -> string
val pprint_typeinfo_int_nts_var_list : Nts_types.nts_var list -> string
val pprint_typeinfo_nts_var_list : Nts_types.nts_var list -> string
