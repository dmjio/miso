#ifndef MISO_RELOAD_H
#define MISO_RELOAD_H

/* The pointer store of Miso.Reload (cbits/foreign.c). */
void miso_x_store(void *ptr);
void *miso_x_get(void);
int miso_x_exists(void);
void miso_x_clear(void);

#endif
