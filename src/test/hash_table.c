/* hash table */

#include <stdio.h>
#include <stdlib.h>

/* modify these lines to establish data type */
typedef int T;                  /* type of item to be stored */
typedef int hashTableIndex;     /* index into hash table */
#define compEQ(a,b) (a == b)

typedef struct Node_ {
	struct Node_ *next;         /* next node */
	T data;                     /* data stored in node */
} Node;


Node **hashTable;
int hashTableSize;

hashTableIndex hash(T data) {

	/***********************************
	 *  hash function applied to data  *
	 ***********************************/

	return (data % hashTableSize);
}

Node *insertNode(T data) {
	Node *p, *p0;
	hashTableIndex bucket;

	/************************************************
	 *  allocate node for data and insert in table  *
	 ************************************************/

	/* insert node at beginning of list */
	bucket = hash(data);
	if ((p = malloc(sizeof(Node))) == 0) {
		fprintf (stderr, "out of memory (insertNode)\n");
		exit(1);
	}
	p0 = hashTable[bucket];
	hashTable[bucket] = p;
	p->next = p0;
	p->data = data;
	return p;
}

void deleteNode(T data) {
	Node *p0, *p;
	hashTableIndex bucket;

	/********************************************
	 *  delete node containing data from table  *
	 ********************************************/

	/* find node */
	p0 = 0;
	bucket = hash(data);
	p = hashTable[bucket];
	while (p && !compEQ(p->data, data)) {
		p0 = p;
		p = p->next;
	}
	if (!p) return;

	/* p designates node to delete, remove it from list */
	if (p0)
		/* not first node, p0 points to previous node */
		p0->next = p->next;
	else
		/* first node on chain */
		hashTable[bucket] = p->next;

	free (p);
}

Node *findNode (T data) {
	Node *p;

	/*******************************
	 *  find node containing data  *
	 *******************************/

	p = hashTable[hash(data)];
	while (p && !compEQ(p->data, data)) 
		p = p->next;
	return p;
}

int main(int argc, char **argv) {
	int i, *a, maxnum, random;

	/* command-line:
	 *
	 *   has maxnum hashTableSize [random]
	 *
	 *   has 2000 100
	 *       processes 2000 records, tablesize=100, sequential numbers
	 *   has 4000 200 r
	 *       processes 4000 records, tablesize=200, random numbers
	 *
	 */
	maxnum = atoi(argv[1]);
	hashTableSize = atoi(argv[2]);
	random = argc > 3;

	if ((a = malloc(maxnum * sizeof(*a))) == 0) {
		fprintf (stderr, "out of memory (a)\n");
		exit(1);
	}

	if ((hashTable = malloc(hashTableSize * sizeof(Node *))) == 0) {
		fprintf (stderr, "out of memory (hashTable)\n");
		exit(1);
	}

	if (random) { /* random */
		/* fill "a" with unique random numbers */
		for (i = 0; i < maxnum; i++) a[i] = rand();
		printf ("ran ht, %d items, %d hashTable\n", maxnum, hashTableSize);
	} else {
		for (i=0; i<maxnum; i++) a[i] = i;
		printf ("seq ht, %d items, %d hashTable\n", maxnum, hashTableSize);
	}


	for (i = 0; i < maxnum; i++) {
		insertNode(a[i]);
	}

	for (i = maxnum-1; i >= 0; i--) {
		findNode(a[i]);
	}

	for (i = maxnum-1; i >= 0; i--) {
		deleteNode(a[i]);
	}
	return 0;
}
