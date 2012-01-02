#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "fifo.h"

static void printLong(void * e) {
  // %ld tells printf to print a long integer
  printf("%ld", (long) e);
}

static void printString(void * e) {
  printf("%s", (char *) e);
}

int main() {
  fifo longq, stringq;
  void * e;

  // test with longs

  longq = newFifo(3);

  assert(isEmptyFifo(longq));

  printf("longq (empty): ");
  printFifo(longq, printLong);

  assert(!putFifo(longq, (void *) 1));
  assert(!putFifo(longq, (void *) 2));
  assert(!putFifo(longq, (void *) 3));

  assert(putFifo(longq, (void *) 4));

  printf("longq (3 elements): ");
  printFifo(longq, printLong);

  assert(!getFifo(longq, &e));
  printf("from longq (1): %ld\n", (long) e);

  assert(!putFifo(longq, (void *) 4));

  printf("longq (3 elements): ");
  printFifo(longq, printLong);

  assert(!getFifo(longq, &e));
  printf("from longq (2): %ld\n", (long) e);
  assert(!getFifo(longq, &e));
  printf("from longq (3): %ld\n", (long) e);
  assert(!getFifo(longq, &e));
  printf("from longq (4): %ld\n", (long) e);

  assert(isEmptyFifo(longq));
  assert(getFifo(longq, &e));
  assert(!e);

  deleteFifo(longq);

  // test with strings

  stringq = newFifo(3);

  assert(isEmptyFifo(stringq));

  printf("stringq (empty): ");
  printFifo(stringq, printString);

  assert(!putFifo(stringq, (char *) "Hello"));
  assert(!putFifo(stringq, (char *) "there"));
  assert(!putFifo(stringq, (char *) "universe"));

  assert(putFifo(stringq, (char *) "!"));

  printf("stringq (3 elements): ");
  printFifo(stringq, printString);

  assert(!getFifo(stringq, &e));
  printf("from stringq (Hello): %s\n", (char *) e);

  assert(!putFifo(stringq, (char *) "!"));

  printf("stringq (3 elements): ");
  printFifo(stringq, printString);

  assert(!getFifo(stringq, &e));
  printf("from stringq (there): %s\n", (char *) e);
  assert(!getFifo(stringq, &e));
  printf("from stringq (universe): %s\n", (char *) e);
  assert(!getFifo(stringq, &e));
  printf("from stringq (!): %s\n", (char *) e);

  assert(isEmptyFifo(stringq));
  assert(getFifo(stringq, &e));
  assert(!e);

  deleteFifo(stringq);

  return 0;
}
