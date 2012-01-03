#ifndef _COMPLEX_H_
#define _COMPLEX_H_

/* The type declaration of the ADT. */
typedef struct _complex * complex;

/* Creates a complex number, initially 0. */
complex newComplex();

/* Deletes a complex number. */
void deleteComplex(complex c);

// "setters"

/* Sets the real component of c. */
void setReal(complex c, double r);

/* Sets the imaginary component of c. */
void setImaginary(complex c, double i);

// "getters"

/* Returns the real component of c. */
double getReal(complex c);

/* Returns the imaginary component of c. */
double getImaginary(complex c);

// basic arithmetic

/* Adds b to a, with the result being stored in a. */
void addTo(complex a, complex b);

/* Multiplies b by a, with the result being stored in a. */
void multiplyBy(complex a, complex b);

/* Multiplies complex a by real b, with the result being
 * stored in a.
 */
void multiplyByReal(complex a, double b);

/* Prints in a + bi form. */
void printComplex(complex c);

#endif
