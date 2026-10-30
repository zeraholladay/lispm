SRC := src
TEST_SRC := test
BIN := bin
GEN := gen

CC ?= cc
PKG_CONFIG ?= pkg-config
FLEX ?= flex
BISON ?= bison
GPERF ?= gperf
XXD ?= xxd

CPPFLAGS += -Iinclude -I$(GEN) -DYYDEBUG=0 $(shell $(PKG_CONFIG) --cflags readline)
ifeq ($(DEBUG),1)
  CFLAGS ?= -std=c11 -Wall -Wextra -O0 -g
  CPPFLAGS += -Dlispm_DEBUG
else
  CFLAGS ?= -std=c11 -Wall -Wextra -O2 -DNDEBUG
endif
LDLIBS += $(shell $(PKG_CONFIG) --libs readline)
TEST_FLAGS := $(shell $(PKG_CONFIG) --cflags check)
TEST_LIBS := $(shell $(PKG_CONFIG) --libs check) -pthread

EXEC := $(BIN)/lispm
TEST_EXE := $(BIN)/test_main
CORE_SOURCES := $(filter-out $(SRC)/main.c,$(wildcard $(SRC)/*.c))
CORE_OBJECTS := $(patsubst $(SRC)/%.c,$(BIN)/src/%.o,$(CORE_SOURCES))
GENERATED_OBJECTS := $(addprefix $(BIN)/gen/,bison.o flex.o keywords.o)
RUNTIME_OBJECTS := $(CORE_OBJECTS) $(GENERATED_OBJECTS)
MAIN_OBJECT := $(BIN)/src/main.o
TEST_OBJECTS := $(patsubst $(TEST_SRC)/%.c,$(BIN)/test/%.o,$(wildcard $(TEST_SRC)/*.c))
ALL_OBJECTS := $(RUNTIME_OBJECTS) $(MAIN_OBJECT) $(TEST_OBJECTS)

BISON_C := $(GEN)/bison.c
BISON_H := $(GEN)/bison.h
FLEX_C := $(GEN)/flex.c
FLEX_H := $(GEN)/flex.h
KEYWORDS_C := $(GEN)/keywords.c
PRELUDE_DATA := $(GEN)/prelude.inc
BUILD_CONFIG := $(BIN)/build-config

.PHONY: all src exec gen test lint clean FORCE generated-directory
all: $(EXEC)
exec: $(EXEC)
src: $(RUNTIME_OBJECTS)
gen: $(BISON_C) $(BISON_H) $(FLEX_C) $(FLEX_H) $(KEYWORDS_C) $(PRELUDE_DATA)

# Link objects only. Each source, including each entry point and generated
# source, is compiled once with its own automatic header dependency file.
$(EXEC): $(RUNTIME_OBJECTS) $(MAIN_OBJECT)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $^ $(LDLIBS)

$(TEST_EXE): $(RUNTIME_OBJECTS) $(TEST_OBJECTS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $^ $(TEST_LIBS) $(LDLIBS)

test: $(TEST_EXE)
	@./$(TEST_EXE)

$(BIN)/src/%.o: $(SRC)/%.c $(BUILD_CONFIG) | $(BIN)/src
	$(CC) $(CPPFLAGS) $(CFLAGS) -MMD -MP -c $< -o $@

$(BIN)/gen/%.o: $(GEN)/%.c $(BUILD_CONFIG) | $(BIN)/gen
	$(CC) $(CPPFLAGS) $(CFLAGS) -MMD -MP -c $< -o $@

$(BIN)/test/%.o: $(TEST_SRC)/%.c $(BUILD_CONFIG) | $(BIN)/test
	$(CC) $(CPPFLAGS) $(TEST_FLAGS) $(CFLAGS) -MMD -MP -c $< -o $@

# Bootstrap prerequisites are explicit because .d files do not exist on a
# first build. The generated-header rules also recover a missing header when
# its generated C file still exists, without racing generators under make -j.
$(BIN)/src/parser.o $(BIN)/gen/bison.o $(BIN)/gen/flex.o: $(BISON_H) $(FLEX_H)
$(BIN)/src/prelude.o: $(PRELUDE_DATA)

$(BISON_C): $(SRC)/bison.y | generated-directory
	$(BISON) $(BISON_FLAGS) -o $@ --defines=$(BISON_H) $<
$(BISON_H): $(BISON_C)
	@test -f $@ || $(BISON) $(BISON_FLAGS) -o $(BISON_C) --defines=$@ $(SRC)/bison.y

$(FLEX_C): $(SRC)/flex.l $(BISON_H) | generated-directory
	$(FLEX) $(FLEX_FLAGS) -o $@ --header-file=$(FLEX_H) $<
$(FLEX_H): $(FLEX_C)
	@test -f $@ || $(FLEX) $(FLEX_FLAGS) -o $(FLEX_C) --header-file=$@ $(SRC)/flex.l

$(KEYWORDS_C): $(SRC)/keywords.g | generated-directory
	$(GPERF) $< --output-file=$@

$(PRELUDE_DATA): lib/prelude.scm | generated-directory
	$(XXD) -i < $< > $@
	@printf ', 0\n' >> $@

# Keep the timestamp when configuration is unchanged. Changing compiler,
# flags, DEBUG, or libraries therefore rebuilds objects without a make clean,
# while an unchanged make all really is a no-op.
$(BUILD_CONFIG): FORCE | $(BIN)
	@printf '%s\n' '$(CC)' '$(CPPFLAGS)' '$(CFLAGS)' '$(LDFLAGS)' '$(LDLIBS)' '$(TEST_FLAGS)' '$(TEST_LIBS)' > $@.tmp
	@if cmp -s $@.tmp $@; then rm $@.tmp; else mv $@.tmp $@; fi

$(BIN) $(BIN)/src $(BIN)/gen $(BIN)/test:
	@mkdir -p $@

generated-directory:
	@mkdir -p $(GEN)

lint: clean
	clang-format -i src/*.c include/*.h test/*.c src/*.y
	cppcheck src/*.c include/*.h test/*.c --check-level=exhaustive --error-exitcode=1

clean:
	@rm -rf $(BIN) $(GEN)

-include $(ALL_OBJECTS:.o=.d)
