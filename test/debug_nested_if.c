#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>

void assert_goku(bool condition, const char* message);

int fibonacci(int n) {
    return ((n == 0) ? 0 : ((n == 1) ? 1 : (fibonacci((n - 1)) + fibonacci((n - 2)))));
}

int main() {
    // Context for variables (simple for now, could be a hash map)
    // For simplicity, all variables are int for now.
    // In a real compiler, types would be tracked.
    assert_goku((fibonacci(5) == 5), "(fibonacci(5) == 5)");

    return 0;
}

// Helper for assertions
void assert_goku(bool condition, const char* message) {
    if (!condition) {
        fprintf(stderr, "Assertion failed! %s\n", message);
        exit(1);
    }
}
