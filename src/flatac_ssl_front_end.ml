(**
This file specializes the generic front end, in order to extract a
counter automata based model which nodes are labelled with values
of the SSL logic. 
*)
open Intermediate_language
open Cautomata
open Cil_types
open Sem_and_logic_front_end
open Ssl_types
open Cautomata



class ssl_flatac_front_end = object (self)
  inherit [SSL_lex.formula , Cautomata.trans_label list ]  sem_and_logic_front_end

    
end


