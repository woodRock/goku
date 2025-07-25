#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

void assert_goku(bool condition, const char* message);
char* concat_strings(const char* s1, const char* s2);

int precedencetest(int a, int b, int c) {
    return (a + (b * c));
}
int complexcalc(int x) {
    return (((2 * x) + (3 * x)) - (x / 2));
}

int main() {
    // Context for variables (simple for now, could be a hash map)
    // For simplicity, all variables are int for now.
    // In a real compiler, types would be tracked.
    assert_goku((((1 + 2) - 3) == 0), "(((1 + 2) - 3) == 0)");

    assert_goku((((10 - 5) + 2) == 7), "(((10 - 5) + 2) == 7)");

    assert_goku(((2 + (3 * 4)) == 14), "((2 + (3 * 4)) == 14)");

    assert_goku((((3 * 4) + 2) == 14), "(((3 * 4) + 2) == 14)");

    assert_goku(((10 - (2 * 3)) == 4), "((10 - (2 * 3)) == 4)");

    assert_goku((((2 * 3) - 4) == 2), "(((2 * 3) - 4) == 2)");

    assert_goku((((8 / 2) * 3) == 12), "(((8 / 2) * 3) == 12)");

    assert_goku((((12 / 3) / 2) == 2), "(((12 / 3) / 2) == 2)");

    assert_goku((((2 * 6) / 3) == 4), "(((2 * 6) / 3) == 4)");

    assert_goku((((10 / 3) * 2) == 6), "(((10 / 3) * 2) == 6)");

    assert_goku((((15 / 3) / 2) == 2), "(((15 / 3) / 2) == 2)");

    assert_goku((((1 + (2 * 3)) - 4) == 3), "(((1 + (2 * 3)) - 4) == 3)");

    assert_goku((((20 / 4) + (2 * 3)) == 11), "(((20 / 4) + (2 * 3)) == 11)");

    assert_goku(((2 + ((3 * 4) / 2)) == 8), "((2 + ((3 * 4) / 2)) == 8)");

    assert_goku((((2 + 3) * 4) == 20), "(((2 + 3) * 4) == 20)");

    assert_goku(((2 * (3 + 4)) == 14), "((2 * (3 + 4)) == 14)");

    assert_goku((((10 - 2) / 4) == 2), "(((10 - 2) / 4) == 2)");

    assert_goku(((20 / (2 + 3)) == 4), "((20 / (2 + 3)) == 4)");

    assert_goku((((2 + 3) * (4 - 1)) == 15), "(((2 + 3) * (4 - 1)) == 15)");

    assert_goku((((10 / 2) + (3 * 2)) == 11), "(((10 / 2) + (3 * 2)) == 11)");

    assert_goku((((2 + (3 * 4)) - (5 / 5)) == 13), "(((2 + (3 * 4)) - (5 / 5)) == 13)");

    assert_goku(((((1 + (2 * 3)) + (4 * 5)) - 6) == 21), "((((1 + (2 * 3)) + (4 * 5)) - 6) == 21)");

    assert_goku((precedencetest(2, 3, 4) == 14), "(precedencetest(2, 3, 4) == 14)");

    assert_goku((complexcalc(4) == 18), "(complexcalc(4) == 18)");

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
