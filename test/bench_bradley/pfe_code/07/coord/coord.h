#ifndef _COORD_H_
#define _COORD_H_

/* The type declaration of the ADT. */
typedef struct _coord * coord;

/* Creates a coordinate. */
coord newCoord();

/* Deletes a coordinate. */
void deleteCoord(coord c);

// "getters"

/* For Cartesian coordinates. */
double getX(coord c);
double getY(coord c);

/* Returns the radius component. */
double getR(coord c);

/* Returns the angle component through the reference th.  
 * The angle is undefined if the corresponding Cartesian 
 * coordinate is (0, 0), so it returns -1 in this case;
 * otherwise, it returns 0.
 */
int getTheta(coord c, double * th);

// "setters"

/* For Cartesian coordinates. */
void setX(coord c, double x);
void setY(coord c, double y);

/* Set the radius/angle components if possible and return 
 * 0.  However, neither can be set if the corresponding 
 * Cartesian coordinate is (0, 0), so they leave the 
 * coordinate unmodified and return -1 in this case.
 */
int setR(coord c, double r);
int setTheta(coord c, double th);

#endif
