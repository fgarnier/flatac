#include <stdlib.h>

int GetRandomBetweenRange(int min, int max) {
	return ((rand() * max) + min) % max;
}
int GetRandomValue() {
	return GetRandomBetweenRange(1, 57);
}

int main() {
	int i = GetRandomValue();
	while(i < 50) i = GetRandomValue();

	return 0;
}
