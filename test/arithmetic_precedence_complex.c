#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

void assert_goku(bool condition, const char* message);
char* concat_strings(const char* s1, const char* s2);


int main() {
    // Context for variables (simple for now, could be a hash map)
    // For simplicity, all variables are int for now.
    // In a real compiler, types would be tracked.
    assert_goku(((2 + (3 * 4)) == 14), "((2 + (3 * 4)) == 14)");

    assert_goku(((10 - (2 * 3)) == 4), "((10 - (2 * 3)) == 4)");

    assert_goku((((10 - 5) + 2) == 7), "(((10 - 5) + 2) == 7)");

    assert_goku((((8 / 2) * 3) == 12), "(((8 / 2) * 3) == 12)");

    assert_goku((((2 + 3) * 4) == 20), "(((2 + 3) * 4) == 20)");

    return 0;
}

// Helper for assertions
void assert_goku(bool condition, const char* message) {
    if (!condition) {
        fprintf(stderr, "Assertion failed! %s\n", message);
        exit(1);
    }
}

// Helper for string concatenation
char* concat_strings(const char* s1, const char* s2) {
    if (!s1 || !s2) return NULL;
    size_t len1 = strlen(s1);
    size_t len2 = strlen(s2);
    char* result = malloc(len1 + len2 + 1);
    if (!result) return NULL;
    strcpy(result, s1);
    strcat(result, s2);
    return result;
}
