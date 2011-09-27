
#ifndef BUFFER_H
#define BUFFER_H

enum {
  OK,
  KO_NOMEM, 
  KO_UNDERFLOW, 
  KO_OVERFLOW
};

/*
 * circular buffer structure
 *
 */

typedef struct {
  char* head;
  char* out;
  char* in;
  int size;
  int free;
  int count;
} buffer_t;

/*
 * basic operations
 *
 */
  
int init(buffer_t*, int);

int put(buffer_t*, int, char*);

int get(buffer_t*, int*, char**);

int dispose(buffer_t*);

void trace(const char*);

#endif
