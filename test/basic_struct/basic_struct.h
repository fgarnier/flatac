
#ifndef __FLATAC_BASIC_STRUCT_H__
#define __FLATAC_BASIC_STRUCT_H__

 enum mounth{
  Vendemiaire,
  Brumaire,
  Frimaire,
  Nivose,
  Pluviose,
  Ventose,
  Germinal,
  Floreal,
  Prairial, 
  Messidor,
  Thermidorn,
  Fructidor,

 };

 enum gender_type{
  male,
  female,
 };

 typedef struct {

 int year_of_birth;
 int day_of_birth;
 
 enum mounth mounth_of_birth;
 enum gender_type sex;

 char *name; /**/
 char *firstname;
 } personne;





#endif /* This marks the end of the ifndef __FLATAC_BASIC_STRUCT_H__ 
 macro block as well as the EOF*/
