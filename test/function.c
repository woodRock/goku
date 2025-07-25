#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>

void assert_goku(bool condition, const char* message);

int id(int x) {
    return x;
}
int id2(int x) {
    return x;
}
int add(int x, int y) {
    return (x + y);
}
int add2(int x, int y) {
    return (x + y);
}

int main() {
    // Context for variables (simple for now, could be a hash map)
    // For simplicity, all variables are int for now.
    // In a real compiler, types would be tracked.
    assert_goku((id(1) == 1), "(id(1) == 1)");

    assert_goku((id2(1) == 1), "(id2(1) == 1)");

    assert_goku((add(1, 1) == 2), "(add(1, 1) == 2)");

    assert_goku((add2(1, 1) == 2), "(add2(1, 1) == 2)");

    return 0;
}

// Helper for assertions
void assert_goku(bool condition, const char* message) {
    if (!condition) {
        fprintf(stderr, "Assertion failed! %s\n", message);
        exit(1);
    }
}
