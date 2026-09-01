
int foo(int x) {
	for (int i = 0; i < 10*x; i++) {
		x += 1;
	}
	return x;
}

int main() {
	foo(0);
	return 0;
}
