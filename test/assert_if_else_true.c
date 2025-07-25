#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>

void assert_goku(bool condition, const char* message);


int main() {
    // Context for variables (simple for now, could be a hash map)
    // For simplicity, all variables are int for now.
    // In a real compiler, types would be tracked.
    if (true) {
        assert_goku(true, "true");
    } else {
        assert_goku(false, "false");
    }

    return 0;
}

// Helper for assertions
void assert_goku(bool condition, const char* message) {
    if (!condition) {
        fprintf(stderr, "Assertion failed! %s\n", message);
        exit(1);
    }
}
