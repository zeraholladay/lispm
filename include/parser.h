#ifndef PARSER_H
#define PARSER_H

#include <stdio.h>

#include "lm.h"
#include "types.h"

void *parser_loads (const char *str, size_t len);
void *parser_load (const char *fname);

void parser_destroy (void);

bool parser_parse_bytes (void *ptr, Cell **progn, LM *lm);

#endif
