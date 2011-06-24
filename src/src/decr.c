int decr( int x ) {
	if( x > 0 ) x -= 1; 
	else x = 0;

	return x;
}

int decr2 ( int x ) {
	while ( x > 0 ) x -= 1;

	return x;
}

int decr3( int x ) {
	if( x > 0 ) return decr3(x - 1);
	else x = 0;

	return x;
}
