#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>

void assert_goku(bool condition, const char* message);

int factorial(int n) {
    return ((n == 0) ? 1 : (n * factorial((n - 1))));
}
int fibonacci(int n) {
    return ((n == 0) ? 0 : ((n == 1) ? 1 : (fibonacci((n - 1)) + fibonacci((n - 2)))));
}

int main() {
    // Context for variables (simple for now, could be a hash map)
    // For simplicity, all variables are int for now.
    // In a real compiler, types would be tracked.
    assert_goku((factorial(0) == 1), "(factorial(0) == 1)");

    assert_goku((factorial(1) == 1), "(factorial(1) == 1)");

    assert_goku((factorial(3) == 6), "(factorial(3) == 6)");

    assert_goku((factorial(5) == 120), "(factorial(5) == 120)");

    assert_goku((fibonacci(0) == 0), "(fibonacci(0) == 0)");

    assert_goku((fibonacci(1) == 1), "(fibonacci(1) == 1)");

    assert_goku((fibonacci(2) == 1), "(fibonacci(2) == 1)");

    assert_goku((fibonacci(3) == 2), "(fibonacci(3) == 2)");

    assert_goku((fibonacci(5) == 5), "(fibonacci(5) == 5)");

    assert_goku((fibonacci(7) == 13), "(fibonacci(7) == 13)");

    return 0;
}

// Helper for assertions
void assert_goku(bool condition, const char* message) {
    if (!condition) {
        fprintf(stderr, "Assertion failed! %s\n", message);
        exit(1);
    }
}
