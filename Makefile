# export DEBUG=1 # enable debugging
# make clean; make test
#
# make hints
# $@	The target of the rule (the thing being built)
# $<	The first prerequisite (typically the input file)
# $^	All prerequisites, with duplicates removed

# src

# src/ and gen/
SRC := src
BIN := bin
GEN := gen

MAIN_SRC := $(SRC)/repl.c
EXEC := $(BIN)/lispm

# Compilers and flags
CC ?= gcc

ifeq ($(DEBUG), 1)
	CFLAGS := -Iinclude -I$(GEN) -Wall -Wextra -O0 -g -DYYDEBUG=0 -Dlispm_DEBUG
	# FLEX_FLAGS := -d
	# BISON_FLAGS := -d -v --debug
else
	CFLAGS := -Iinclude -I$(GEN) -Wall -Wextra -O2 -DNDEBUG -DYYDEBUG=0
	FLEX_FLAGS :=
	BISON_FLAGS :=
endif

# libs
ifeq ($(shell uname), Darwin)
	LDLIBS   += $(shell pkg-config --cflags --libs readline)
	LDFLAGS  += $(shell pkg-config --cflags --libs readline)
endif

FLEX := flex
BISON := bison
GPERF := gperf

FLEX_SRC := $(SRC)/flex.l
BISON_SRC := $(SRC)/bison.y
GPERF_SCR := $(SRC)/keywords.gperf

FLEX_C := $(GEN)/flex.c
FLEX_H := $(GEN)/flex.h
BISON_C := $(GEN)/bison.c
BISON_H := $(GEN)/bison.h
GPERF_C := $(GEN)/keywords.c

SRC_CFILES := $(wildcard $(SRC)/*.c) $(FLEX_C) $(BISON_C) $(GPERF_C)
SRC_CFILES_ALL := $(sort $(SRC_CFILES))
SRC_OBJS := $(patsubst $(SRC)/%.c, $(BIN)/%.o, $(SRC_CFILES_ALL))

.PHONY: all
all: src exec

exec: $(MAIN_SRC)
	$(CC) $(CFLAGS) -DLISPM_MAIN=1 -o $(EXEC) $(SRC_OBJS) $(MAIN_SRC) $(LDLIBS)

.PHONY: src
src: gen $(SRC_OBJS)

# ie pattern rule istead of $(SRC_OBJS) because $(SRC_OBJS) would assume gcc built all $(SRC_OBJS)
$(BIN)/%.o: $(SRC)/%.c | bin
	$(CC) $(CFLAGS) -c $< -o $@

# gen
.PHONY: gen
gen: $(BISON_C) $(FLEX_C) $(GPERF_C)

$(BISON_C): $(BISON_SRC) | bin
	$(BISON) $(BISON_FLAGS) -o $(BISON_C) --defines=$(BISON_H) $(BISON_SRC)

$(FLEX_C): $(FLEX_SRC) $(BISON_H) | bin
	$(FLEX) $(FLEX_FLAGS) -o $(FLEX_C) --header-file=$(FLEX_H) $(FLEX_SRC)

$(GPERF_C): $(GPERF_SCR) $(BISON_H) | bin
	$(GPERF) $(GPERF_SCR) --output-file=$(GPERF_C)

# test/
TEST_SRC := test

TEST_MAIN_SRC := $(TEST_SRC)/test_main.c
TEST_EXE := $(BIN)/test_main

TEST_CFILES := $(wildcard $(TEST_SRC)/*.c)
TEST_OBJS := $(patsubst $(TEST_SRC)/%.c, $(BIN)/%.o, $(TEST_CFILES))

ifeq ($(shell uname), Darwin)
	CHECK := check
	TEST_FLAGS := $(shell pkg-config --cflags $(CHECK))
	TEST_LIBS := $(shell pkg-config --libs $(CHECK)) -pthread $(LIBS)
endif

.PHONY: test
test: src $(TEST_EXE)
	@./$(TEST_EXE)

$(TEST_EXE): $(TEST_OBJS) | bin
	$(CC) $(CFLAGS) -DTEST_MAIN=1 -o $(TEST_EXE) $(SRC_OBJS) $(TEST_OBJS) $(TEST_MAIN_SRC) $(TEST_FLAGS) $(TEST_LIBS) $(LDLIBS)

$(BIN)/%.o: $(TEST_SRC)/%.c
	$(CC) $(CFLAGS) -c $< -o $@ $(TEST_FLAGS)

# linting
.PHONY: lint
lint: clean
	clang-format --style=GNU -i */*.c */*.y */*.h
	cppcheck */*.c */*.h --check-level=exhaustive

# bin/
.PHONY: bin
bin:
	-@mkdir $(BIN) $(GEN)

# clean
.PHONY: clean
clean:
	-@rm -rf $(FLEX_C) $(BISON_C) $(BISON_H) $(GPERF_C) $(BIN)/* $(GEN)/*
	-@rmdir $(BIN) $(GEN)
