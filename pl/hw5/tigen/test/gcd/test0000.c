#include <stdio.h>

extern int gcd(int m , int n ) ;


int main() {
	int  m;
	int  n;
	alarm(2);
	m = 2;
	n = 1;


	gcd(m, n);
	return 0;
}
