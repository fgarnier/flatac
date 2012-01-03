#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "fifo.h"

typedef struct _node {
  struct _node * next;
  void * e;
} * node;

static node newNode(void * e) {
  node n = (node) malloc(sizeof(struct _node));
  n->next = NULL;
  n->e = e;
  return n;
}

static void deleteNode(node n) {
  assert (n);
  free(n);
}

struct _fifo {
  int capacity;
  int size;
  node head;
  node tail;
};

fifo newFifo(int capacity) {
  fifo q = (fifo) malloc(sizeof(struct _fifo));
  if (capacity <= 0) capacity = -1;
  q->capacity = capacity;
  q->size = 0;
  q->head = NULL;
  q->tail = NULL;
  return q;
}

int isEmptyFifo(fifo q) {
  assert (q);
  return (q->size == 0);
}

int putFifo(fifo q, void * e) {
  assert (q);
  if (q->size == q->capacity)
    // Full?  Impossible if q->capacity == -1.
    return -1;

  node n = newNode(e);
  if (q->size == 0) {
    // Both the head and the tail should be NULL.
    assert (!q->head);
    assert (!q->tail);
    // Set them both to point to n.
    q->head = n;
    q->tail = n;
  }
  else {
    // The tail node should be the last one.
    assert (!q->tail->next);
    // Append n and make it the new tail.
    q->tail->next = n;
    q->tail = n;
  }
  q->size++;

  return 0;
}

int getFifo(fifo q, void ** e) {
  assert (q);
  if (!e) {
    // Nowhere to write result.
    return -1;
  }
  if (isEmptyFifo(q)) {
    // Nothing to get.
    *e = NULL;
    return -2;
  }
  // Should be nonempty at this point.
  assert (q->head);

  node n = q->head;
  // Write the element.
  *e = n->e;
  if (q->size == 1) {
    // n should not have a successor.
    assert (!n->next);
    // Set both head and tail to NULL (empty list).
    q->head = NULL;
    q->tail = NULL;
  }
  else {
    // Set the head to n's successor.
    q->head = n->next;
  }
  deleteNode(n);
  q->size--;

  return 0;
}

int printFifo(fifo q, printFn f) {
  assert (q);
  if (!f) return -1;

  int cnt = 1;
  node n;
  for (n = q->head; n != NULL; n = n->next) {
    // Print the index of the element.
    printf(" %d:", cnt);
    // Call user-provided f to print the element.
    f(n->e);
    cnt++;
  }
  printf("\n");

  return 0;
}

void deleteFifo(fifo q) {
  assert (q);
  node n = q->head;
  while (n != NULL) {
    node next = n->next;
    deleteNode(n);
    n = next;
  }
  free(q);
}
