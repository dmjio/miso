#include <stdlib.h>
#include <stdint.h>

// dmj: like `foreign-store`, but just for a single Ptr.
// this is used to store a StablePtr (IORef a), retrieved after a GHCi reload

static void *stored_value = NULL;

// Store a pointer
void x_store(void *ptr) {
    stored_value = ptr;
}

// Get the stored pointer
void *x_get(void) {
    return stored_value;
}

// Check if a pointer is stored (non-NULL)
int x_exists(void) {
    return stored_value != NULL;
}

// Clear the stored pointer
void x_clear(void) {
    stored_value = NULL;
}
