int main(int argc, char** argv)
{
	int i = 0;

	while (i < 100) {
		if ( i % 2 == 0 ){ i -= 1;}
		else i += 3;
	}

	return 0;
}
