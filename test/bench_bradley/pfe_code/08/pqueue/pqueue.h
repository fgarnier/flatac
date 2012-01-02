#ifndef _PQUEUE_H_
#define _PQUEUE_H_

/* Defines the ADT of Priority Queue. */

typedef struct _pqueue * pqueue;

/* Definition of a comparison function:
 *  -1: e1 has higher priority than e2
 *   0: e1 and e2 have equal priorities
 *   1: e1 has lower priority than e2
 */
typedef int (* compareFn)(void * e1, void * e2);

/* Returns a new pqueue ordered by f. */
pqueue newPQueue(compareFn f);

/* Deletes a pqueue. */
void deletePQueue(pqueue q);

/* Returns 1 if q is empty and otherwise 0. */
int isEmptyPQueue(pqueue q);

/* Adds element e to q. */
void putPQueue(pqueue q, void * e);

/* Sets e to point to the element of q with the highest
 * priority and removes that element from q.  Returns 0 if
 * successful and -1 if e is NULL.  If q is empty, returns
 * -2 and sets *e to NULL.  If there are multiple elements
 * with equal priorities, only one of them is returned, but
 * which one is implementation-dependent.
 */
int getPQueue(pqueue q, void ** e);

/* Prints the elements of q in order.  Requires a pointer 
 * to a function that prints an element.  Returns 0 if 
 * successful and -1 if f is NULL.
 */
typedef void (* printFn)(void *);
int printPQueue(pqueue q, printFn f);

/* Returns the number of values in q whose priorities equal
 * that of e. 
 */
int countPQueue(pqueue q, void * e);

/* Removes all values from q whose priorities are equal to
 * that of e. 
 */
void removePQueue(pqueue q, void * e);

#endif
