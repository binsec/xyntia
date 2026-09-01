
int two(int x, int y) {
	// equiv to x + y
	int a = (x ^ y) + 2 * (x & y);
	if (a != y) return a; 
	else return a - x;
}

int main() {
	two(0, 0);
	return 0;
}
