#include <stdlib.h>


typedef struct {
 char *nom;
 int identifiant;
 struct bidon * bidon_suivant;
} bidon;


int main (int argc, char **argv){

  bidon *gros;
  gros = (bidon *)malloc((7*3)*sizeof(bidon));
  gros = gros + 3;

  free(gros);

  return(0);

}
