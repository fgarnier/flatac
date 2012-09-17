

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "buffer.h"

int main() {
  // printf("sizeof(char) = %d\n", sizeof(char));
  // printf("sizeof(int)  = %d\n", sizeof(int));

  buffer_t *b=malloc(sizeof(buffer_t));

  initb(b, 24);

  while (1) {
    char line[80];
    scanf("%s", line);
    if (strstr(line, "put") == line) {
      // put the string, including the final '\0'
      printf("main: put '%s' ... \n", line+3);
      put(b, strlen(line)-2, line+3);
    }
    if (strstr(line, "get") == line) {
      char** data; 
	int * size = malloc(sizeof(int));
      if ( get(b, size, data) == OK) {
	printf("main: get '%s' ...\n", data);
	free( (*data) );
      }
    }
    if (strstr(line, "end") == line)
      break;
  }

  return 0;
}
