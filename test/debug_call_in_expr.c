#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>

void assert_goku(bool condition, const char* message);

int twice(int x) {
    return (x * 2);
}
int test(int n) {
    return (n + twice(5));
}

int main() {
    // Context for variables (simple for now, could be a hash map)
    // For simplicity, all variables are int for now.
    // In a real compiler, types would be tracked.
    assert_goku((test(1) == 11), "(test(1) == 11)");

    return 0;
}

// Helper for assertions
void assert_goku(bool condition, const char* message) {
    if (!condition) {
        fprintf(stderr, "Assertion failed! %s\n", message);
        exit(1);
    }
}
