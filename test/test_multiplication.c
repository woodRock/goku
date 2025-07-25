#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>

void assert_goku(bool condition, const char* message);


int main() {
    // Context for variables (simple for now, could be a hash map)
    // For simplicity, all variables are int for now.
    // In a real compiler, types would be tracked.
    assert_goku(((2 * 3) == 6), "((2 * 3) == 6)");

    assert_goku(((5 * 4) == 20), "((5 * 4) == 20)");

    assert_goku(((1 * 1) == 1), "((1 * 1) == 1)");

    return 0;
}

// Helper for assertions
void assert_goku(bool condition, const char* message) {
    if (!condition) {
        fprintf(stderr, "Assertion failed! %s\n", message);
        exit(1);
    }
}
