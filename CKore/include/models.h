#ifndef MODELS_H
#define MODELS_H

#include <stdint.h>

typedef struct {
    int id;
    char *name;
    char *email;
} User;

typedef struct {
    int id;
    int user_id;
    char *title;
    char *description;
    int is_done;
} Task;

#endif
