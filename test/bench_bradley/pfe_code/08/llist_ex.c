#include <assert.h>
#include <stdlib.h>

typedef struct _node {
  struct _node * next;
  void * e;
} * node;

typedef struct _llist {
  node head;
} * llist;

/* Type of user-defined comparison function.  Should return
 *  -1 - first element is less than second
 *   0 - the two elements are equally valued
 *   1 - the second element is greater than the first
 */  
typedef int (* compareFn)(void *, void *);  // Ex 8.1

// Ex 8.2
int isSorted(llist ll, compareFn f) {
  node n = ll->head;

  // empty?
  if (!n) return 1;
  // single element?
  if (!n->next) return 1;

  if (!f) return 0;

  while (n->next) {
    // If any adjacent pair are in the wrong order...
    if (f(n->e, n->next->e) > 0)
      // ... the list is not sorted.
      return 0;
    n = n->next;
  }
  // All adjacent pairs are ordered; hence, so is the list.
  return 1;
}

// Ex 8.3
void reverse(llist ll1, llist ll2) {
  node n = ll1->head;
  while (n) {
    node next = n->next;
    n->next = ll2->head;
    ll2->head = n;
    n = next;
  }
  ll1->head = NULL;
}

// Ex 8.4
void concat(llist ll1, llist ll2) {
  if (!ll1->head)
    ll1->head = ll2->head;
  else {
    node n;
    // position n so that it points to the final node
    for (n = ll1->head; n->next; n = n->next);
    // now append ll2
    n->next = ll2->head;
  }
  ll2->head = NULL;
}

// Ex 8.14
void concat2(llist ll1, llist ll2) {
  node n = ll1->head;
  node * np = &(ll1->head);  // np chases n
  while (n) {
    // go to the end of ll1
    np = &(n->next);         // np continues to chase n
    n = n->next;
  }
  // np points to the next field of the final node of ll1
  *np = ll2->head;
  ll2->head = NULL;
}

static node newNode(void * e) {
  node n = (node) malloc(sizeof(struct _node));
  n->next = NULL;
  n->e = e;
  return n;
}

// Ex 8.16
llist copy(llist l) {
  // create the new linked list
  llist cl = (llist) malloc(sizeof(struct _llist));
  cl->head = NULL;

  // copy each node of l and add to cl
  node n = l->head;
  // np "chases" the node to be created
  node * np = &(cl->head);  
  while (n) {
    node cn = newNode(n->e);
    *np = cn;
    n = n->next;
    np = &(cn->next);
  }
  return cl;
}

// unit test

int compareLong(void * e1, void * e2) {
  if ((long) e1 < (long) e2) return -1;
  if (e1 == e2) return 0;
  return 1;
}

llist newList() {
  llist l = (llist) malloc(sizeof(struct _llist));
  l->head = NULL;
  return l;
}

int length(llist l) {
  node n;
  int i = 0;
  for (n = l->head; n; n = n->next, ++i);
  return i;
}

int main() {
  llist l1 = newList();
  llist l2 = newList();

  long i;
  for (i = 10; i >= 0; --i) {
    node n = newNode((void *) i);
    llist l = i % 2 ? l1 : l2;
    n->next = l->head;
    l->head = n;
  }

  assert (isSorted(l1, compareLong));
  assert (isSorted(l2, compareLong));

  llist l3 = newList();
  reverse(l1, l3);
  assert (!l1->head);
  assert (!isSorted(l3, compareLong));
  reverse(l3, l1);
  assert (l1->head);
  assert (!l3->head);
  assert (isSorted(l1, compareLong));

  llist l4 = copy(l1);
  assert (length(l1) == length(l4));
  node n1, n2;
  for (n1 = l1->head, n2 = l4->head; n1; n1 = n1->next, n2 = n2->next)
    assert (n1->e == n2->e);
  llist l5 = copy(l2);
  assert (length(l2) == length(l5));

  int a = length(l1);
  int b = length(l2);
  concat(l1, l2);
  assert (length(l1) == a + b);
  assert (length(l2) == 0);

  concat2(l4, l5);
  assert (length(l4) == a + b);
  assert (length(l5) == 0);

  return 0;
}
