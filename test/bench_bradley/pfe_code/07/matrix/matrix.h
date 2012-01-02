#ifndef _MATRIX_H_
#define _MATRIX_H_

/* The type declaration of the ADT. */
typedef struct _matrix * matrix;

/* Creates a rows x cols matrix with all values 0. */
matrix newMatrix(int rows, int cols);

/* Copies a matrix. */
matrix copyMatrix(matrix mtx);

/* Deletes a matrix. */
void deleteMatrix(matrix mtx);

/* Sets the (row, col) element of mtx to val.  Returns 0 if
 * successful, and -1 if row or col are outside of the 
 * dimensions of mtx.
 */
int setElement(matrix mtx, int row, int col, double val);

/* Sets the reference val to value of the (row, col) 
 * element of mtx.  Returns 0 if successful, -1 if val is
 * NULL, and -2 if row or col are outside of the dimensions 
 * of mtx.
 */
int getElement(matrix mtx, int row, int col, double * val);

/* Returns the number of rows of mtx. */
int nRows(matrix mtx);

/* Returns the number of columns of mtx. */
int nCols(matrix mtx);

/* Prints the matrix to stdout. */
void printMatrix(matrix mtx);

/* Writes the transpose of matrix in into matrix out.  
 * Returns 0 if successful, and -1 if the dimensions of in 
 * and out are incompatible.
 */
int transpose(matrix in, matrix out);

/* Writes the sum of matrices mtx1 and mtx2 into matrix sum.
 * Returns 0 if successful, and -1 if the dimensions of the 
 * matrices are incompatible.
 */
int sum(matrix mtx1, matrix mtx2, matrix sum);

/* Writes the product of matrices mtx1 and mtx2 into matrix
 * prod.  Returns 0 if successful, and -1 if the dimensions 
 * of the matrices are incompatible.
 */
int product(matrix mtx1, matrix mtx2, matrix prod);

/* Writes the dot product of vectors v1 and v2 into 
 * reference prod.  Returns 0 if successful, -1 if prod is
 * NULL, -2 if either matrix is not a vector, and -3 if 
 * the vectors are of incompatible dimensions.
 */
int dotProduct(matrix v1, matrix v2, double * prod);

#endif
