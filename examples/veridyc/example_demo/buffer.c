
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <assert.h>

#include "buffer.h"

/*
 *
 *
 */

int initb(buffer_t* b, int size) {
  // make size a multiple of 4
  //size += (size % 4) ? (4 - size % 4) : 0;
  // allocate memory
  /*b->size = size;*/
  
	b->head = malloc( b->size );
  /*if (b->head == NULL) {
    trace("init: aborted, not enough memory");
    return KO_NOMEM;
  }

  // assert 4 byte alignement for the head
  assert ( ((int) b->head) % 4 == 0);
  
  // initialize variables
  b->in = b->out = b->head;
  b->free = b->size;
  b->count = 0;

  trace("init: ok");
  return OK;*/
}

/*
 *
 *
 */

int put(buffer_t* b, int size, char* data) {
  // check for space
  if (b->free < 4 + size) {
    trace("put: aborted, overflow");
    return KO_OVERFLOW;
  }
  // write the length on 4 bytes
  if (b->out + 4 <= b->head + b->size) {
    * ((int*)(b->out)) = size;
    b->out += 4; 
    b->free -= 4;
    // cycle, if needed
    if (b->out == b->head + b->size) 
      b->out = b->head;
  }
  // write the data on 'size' bytes
  if (b->out + size <= b->head + b->size) {
    // data fits entirely
    memcpy(b->out, data, size);
    b->out += size; 
  }
  else {
    // data needs to be 'fractioned' in two parts...
    int s = (b->head + b->size) - b->out;
    memcpy(b->out, data, s);
    memcpy(b->head, data + s, size - s);
    b->out = b->head + size - s;
  }
  // update for free space
  b->free -= size;
  b->count++; 
  // re-align on 4 bytes
  if (size % 4) {
    b->out += (4 - size % 4);
    b->free -= (4 - size % 4);
  }
  // cycle, if needed
  if (b->out == b->head + b->size)
    b->out = b->head;

  trace("put: ok");
  return OK;
}

/*
 *
 *
 */

int get(buffer_t* b, int* size, char** data) {
  if (b->count == 0) {
    trace("get: aborted, underflow");
    return KO_UNDERFLOW;
  }
  // read the data size
  *size = * ((int* ) b->in);
  b->in += 4;
  b->free += 4;
  // cycle, if needed
  if (b->in == b->head + b->size)
    b->in = b->head;
  //
  // allocate memory and read the data
  *data = malloc( *size );
  if (b->in + (*size) <= b->head + b->size) {
    // data comes in one reading
    memcpy(*data, b->in, *size);
    b->in += *size;
  }
  else {
    // data is split, 
    int s = (b->head + b->size) - b->in;
    memcpy( *data, b->in, s);
    memcpy( *data + s, b->head, *size - s);
    b->in = b->head + (*size - s);
  }
  // update for free space
  b->free += *size;
  b->count--;
  // realign on 4 bytes
  if ( *size % 4 ) {
    b->in += (4 - *size % 4);
    b->free += (4 - *size % 4);
  }
  // cycle, if needed
  if (b->in == b->head + b->size)
    b->in = b->head;

  trace("get: ok");
  return OK;
}

/*
 *
 *
 */

int dispose(buffer_t* b) {
  free(b->head);
  return OK;
}

/*
 *
 *
 */

void trace(const char* msg) {
  printf("*** %s\n", msg);
}
