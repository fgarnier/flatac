int main (int argc, char ** argv){

	int res=0;
	int toto = 0;
	res = 3*argc;
	res = res*3;
	res = 4*(res-res*(7+3));
	res = (7+argc)-3*(res-9);
	/*res = (argc+7)*(7+argc);*/
	return (res);


}
