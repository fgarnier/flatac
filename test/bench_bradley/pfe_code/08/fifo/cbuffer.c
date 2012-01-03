#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "fifo.h"

struct _fifo {
  unsigned capacity;
  unsigned head;
  unsigned tail;
  void * data[0];
};

fifo newFifo(int capacity) {
  assert (capacity > 0);

  // The capacity of a circular buffer is one less than one
  // would think: if the user wants a given capacity, the
  // required array is one cell larger.
  capacity++;

  // allocate one chunk of memory
  fifo q = (fifo) malloc(sizeof(struct _fifo) + 
                         capacity * (sizeof(void *)));
  q->capacity = (unsigned) capacity;
  q->head = 0;
  q->tail = 0;
  return q;
}

void deleteFifo(fifo q) {
  assert (q);
  free(q);
}

int isEmptyFifo(fifo q) {
  assert (q);
  return (q->head == q->tail);
}

int putFifo(fifo q, void * e) {
  assert (q);
  if ((q->head+1) % q->capacity == q->tail) // full?
    return -1;
  q->data[q->head] = e;
  q->head = (q->head+1) % q->capacity;
  return 0;
}

int getFifo(fifo q, void ** e) {
  assert (q);
  if (!e) return -1;
  if (isEmptyFifo(q)) {
    *e = NULL;
    return -2;
  }
  *e = q->data[q->tail];
  q->tail = (q->tail+1) % q->capacity;
  return 0;
}

int printFifo(fifo q, printFn f) {
  assert (q);
  if (!f) return -1;

  unsigned i, cnt = 1;
  for (i = q->tail; i != q->head; i = (i+1) % q->capacity) 
  {
    printf(" %d:", cnt);
    f(q->data[i]);
    cnt++;
  }
  printf("\n");
  return 0;
}
