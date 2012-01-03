#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "pqueue.h"

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

struct _pqueue {
  compareFn cmp;
  node head;
};

pqueue newPQueue(compareFn f) {
  pqueue q = (pqueue) malloc(sizeof(struct _pqueue));
  q->cmp = f;
  q->head = NULL;
  return q;
}

void deletePQueue(pqueue q) {
  assert (q);
  node n = q->head;
  while (n) {
    node next = n->next;
    deleteNode(n);
    n = next;
  }
  free(q);
}

int isEmptyPQueue(pqueue q) {
  assert (q);
  return (q->head == NULL);
}

void putPQueue(pqueue q, void * e) {
  assert (q);

  node nn = newNode(e);
  node n = q->head;
  node * np = &(q->head);
  while (n) {
    if (q->cmp(e, n->e) < 0) break;
    np = &(n->next);
    n = n->next;
  }

  nn->next = n;
  *np = nn;
}

int getPQueue(pqueue q, void ** e) {
  assert (q);
  if (!e) return -1;
  if (!q->head) {
    *e = NULL;
    return -2;
  }

  *e = q->head->e;
  node n = q->head;
  q->head = n->next;
  deleteNode(n);

  return 0;
}

int printPQueue(pqueue q, printFn f) {
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

int countPQueue(pqueue q, void * e) {
  int cnt = 0;
  node n;
  for (n = q->head; n; n = n->next)
    if (q->cmp(n->e, e) == 0)
      cnt++;
  return cnt;
}

void removePQueue(pqueue q, void * e) {
  assert (q);

  // Iterate over the list...
  node n = q->head;
  // ... while maintaining a pointer to what points to n.
  node *np = &(q->head);
  while (n) {
    if (q->cmp(n->e, e) == 0) {
      // Remove n.
      *np = n->next;
      deleteNode(n);
      // Advance n...
      n = *np;
      // ... but np is already just behind n.
    }
    else {
      // Advance n and np.
      np = &(n->next);
      n = n->next;
    }
  }
}
