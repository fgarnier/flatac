#include<assert.h>

int halbwachs() {
	int i = 0;
	int j = 0;

	while (i < 50) {
		j = 0;
		while (j < 50) {
			i++;
			j++;
		}
		i = i-j+1;
	}
	assert( i==50 && j == 50);
	return i+j;
}
