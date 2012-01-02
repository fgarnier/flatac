#include <stdio.h>

// Write strcmp here.

int main() {
  printf("aardvark, aardwolf %d\n", 
         strcmp("aardvark", "aardwolf"));
  printf("AVAST, avast %d\n", strcmp("AVAST", "avast"));
  printf("ahoy, ahoy %d\n", strcmp("ahoy", "ahoy"));
  printf("Watch for aardvarks!, "
         "Watches aren't for aardwolves. %d\n",
         strcmp("Watch for aardvarks!",
                "Watches aren't for aardwolves."));
  printf("zoology, zoo %d\n", strcmp("zoology", "zoo"));
  return 0;
}
