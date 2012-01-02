#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "pqueue.h"

// for printPQueue
static void printLong(void * e) {
  printf("%ld", (long) e);
}

// defines priorities over long data
static int compareLong(void * e1, void * e2) {
  if ((long) e1 < (long) e2)
    return -1;
  else if ((long) e1 == (long) e2)
    return 0;
  else
    return 1;
}

int main(int argc, char ** argv) {
  int i, nElements = 5;  // default value for nElements
  pqueue q;

  // Did the user provide an integer argument?
  if (argc > 1) {
    int n;
    if (sscanf(argv[1], "%d", &n))
      // If so, use it as nElements.
      nElements = n;
  }

  q = newPQueue(compareLong);

  // insert nElements random longs
  for (i = 0; i < nElements; ++i) {
    // rand() is provided by stdlib.h
    long e = (long) (rand() % 32);
    printf("putPQueue: %ld\n", e);
    putPQueue(q, (void *) e);
  }

  printf("State of the queue:\n");
  printPQueue(q, printLong);

  // get and print the elements
  while (!isEmptyPQueue(q)) {
    long e;
    assert (!getPQueue(q, (void **) &e));
    printf("getPQueue: %ld\n", e);
  }

  // test countPQueue and removePQueue
  for (i = 0; i < 5; ++i)
    putPQueue(q, (void *) 1);
  printf("\nTest of Ex 8.10 & Ex 8.11\ncountPQueue: %d\n", countPQueue(q, (void *) 1));
  removePQueue(q, (void *) 1);
  assert(isEmptyPQueue(q));

  deletePQueue(q);

  return 0;
}
