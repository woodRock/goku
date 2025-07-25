#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

// List runtime implementation
typedef struct GokuList {
    void** items;
    int length;
    int capacity;
} GokuList;

typedef union GokuValue {
    int int_val;
    bool bool_val;
    char* string_val;
    GokuList* list_val;
} GokuValue;

GokuList* goku_list_new(int count, ...);
GokuList* goku_list_append(GokuList* list, void* item);
GokuList* goku_list_cons(void* item, GokuList* list);
void* goku_list_head(GokuList* list);
GokuList* goku_list_tail(GokuList* list);
bool goku_list_empty(GokuList* list);
int goku_list_length(GokuList* list);
void* goku_list_nth(GokuList* list, int index);
GokuList* goku_list_reverse(GokuList* list);
bool goku_list_elem(void* item, GokuList* list);
GokuList* goku_list_concat(GokuList* list1, GokuList* list2);
void goku_list_free(GokuList* list);
void* goku_int_to_ptr(int value);

void assert_goku(bool condition, const char* message);
char* concat_strings(const char* s1, const char* s2);


int main() {
    // Context for variables (simple for now, could be a hash map)
    // For simplicity, all variables are int for now.
    // In a real compiler, types would be tracked.
    assert_goku(false, "false");

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

// List runtime implementation
GokuList* goku_list_new(int count, ...) {
    GokuList* list = malloc(sizeof(GokuList));
    list->capacity = count == 0 ? 4 : count;
    list->items = malloc(list->capacity * sizeof(void*));
    list->length = 0;
    
    if (count > 0) {
        va_list args;
        va_start(args, count);
        for (int i = 0; i < count; i++) {
            int* item = malloc(sizeof(int));
            *item = va_arg(args, int);
            list->items[list->length++] = item;
        }
        va_end(args);
    }
    return list;
}

GokuList* goku_list_append(GokuList* list, void* item) {
    GokuList* new_list = goku_list_new(0);
    // Copy all items from original list
    for (int i = 0; i < list->length; i++) {
        if (new_list->length >= new_list->capacity) {
            new_list->capacity = new_list->capacity == 0 ? 4 : new_list->capacity * 2;
            new_list->items = realloc(new_list->items, new_list->capacity * sizeof(void*));
        }
        new_list->items[new_list->length++] = list->items[i];
    }
    // Add the new item
    if (new_list->length >= new_list->capacity) {
        new_list->capacity = new_list->capacity == 0 ? 4 : new_list->capacity * 2;
        new_list->items = realloc(new_list->items, new_list->capacity * sizeof(void*));
    }
    new_list->items[new_list->length++] = item;
    return new_list;
}

GokuList* goku_list_cons(void* item, GokuList* old_list) {
    GokuList* list = goku_list_new(0);
    // First add the new item
    if (list->length >= list->capacity) {
        list->capacity = list->capacity == 0 ? 4 : list->capacity * 2;
        list->items = realloc(list->items, list->capacity * sizeof(void*));
    }
    list->items[list->length++] = item;
    // Then add all items from the old list
    for (int i = 0; i < old_list->length; i++) {
        if (list->length >= list->capacity) {
            list->capacity = list->capacity == 0 ? 4 : list->capacity * 2;
            list->items = realloc(list->items, list->capacity * sizeof(void*));
        }
        list->items[list->length++] = old_list->items[i];
    }
    return list;
}

void* goku_list_head(GokuList* list) {
    if (list->length == 0) {
        printf("Error: Cannot take head of empty list\n");
        exit(1);
    }
    return list->items[0];
}

GokuList* goku_list_tail(GokuList* list) {
    if (list->length == 0) {
        printf("Error: Cannot take tail of empty list\n");
        exit(1);
    }
    GokuList* result = goku_list_new(0);
    for (int i = 1; i < list->length; i++) {
        if (result->length >= result->capacity) {
            result->capacity = result->capacity == 0 ? 4 : result->capacity * 2;
            result->items = realloc(result->items, result->capacity * sizeof(void*));
        }
        result->items[result->length++] = list->items[i];
    }
    return result;
}

bool goku_list_empty(GokuList* list) {
    return list->length == 0;
}

int goku_list_length(GokuList* list) {
    return list->length;
}

void* goku_list_nth(GokuList* list, int index) {
    if (index < 0 || index >= list->length) {
        printf("Error: Index %d out of bounds for list of length %d\n", index, list->length);
        exit(1);
    }
    return list->items[index];
}

GokuList* goku_list_reverse(GokuList* list) {
    GokuList* result = goku_list_new(0);
    for (int i = list->length - 1; i >= 0; i--) {
        if (result->length >= result->capacity) {
            result->capacity = result->capacity == 0 ? 4 : result->capacity * 2;
            result->items = realloc(result->items, result->capacity * sizeof(void*));
        }
        result->items[result->length++] = list->items[i];
    }
    return result;
}

bool goku_list_elem(void* item, GokuList* list) {
    for (int i = 0; i < list->length; i++) {
        if (list->items[i] == item) return true;
    }
    return false;
}

GokuList* goku_list_concat(GokuList* list1, GokuList* list2) {
    GokuList* result = goku_list_new(0);
    for (int i = 0; i < list1->length; i++) {
        if (result->length >= result->capacity) {
            result->capacity = result->capacity == 0 ? 4 : result->capacity * 2;
            result->items = realloc(result->items, result->capacity * sizeof(void*));
        }
        result->items[result->length++] = list1->items[i];
    }
    for (int i = 0; i < list2->length; i++) {
        if (result->length >= result->capacity) {
            result->capacity = result->capacity == 0 ? 4 : result->capacity * 2;
            result->items = realloc(result->items, result->capacity * sizeof(void*));
        }
        result->items[result->length++] = list2->items[i];
    }
    return result;
}

void goku_list_free(GokuList* list) {
    if (list) {
        free(list->items);
        free(list);
    }
}

void* goku_int_to_ptr(int value) {
    int* ptr = malloc(sizeof(int));
    *ptr = value;
    return ptr;
}
