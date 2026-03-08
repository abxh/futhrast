.PHONY: all run clean prepare

PROGNAME := futhrast-gui
BUILD_DIR := build

export PROGNAME

CFLAGS ?= -std=gnu11 -O -Wall -Wextra -pedantic
CXXFLAGS ?= -std=c++17 -O -Wall -Wextra -pedantic

# enable ubsan sanitizer. may comment below chunk out.
# CFLAGS += -fsanitize=undefined
# CXXFLAGS += -fsanitize=undefined
# LDFLAGS += -fsanitize=undefined

export CFLAGS CXXFLAGS LDFLAGS

LYS_BACKEND := opencl

export LYS_BACKEND

ll: run

run: build/
	$(MAKE) \
		-C $(BUILD_DIR) \
		-f ../lib/github.com/abxh/lys/common.mk

clean:
	$(MAKE) clean \
		-C $(BUILD_DIR) \
		-f ../lib/github.com/abxh/lys/common.mk

build/:
	@mkdir -p $(BUILD_DIR)
	@for f in *.fut *.bin *.obj; do \
		ln -sf "../$$f" "$(BUILD_DIR)/$$f"; \
	done
	@ln -snf ../lib $(BUILD_DIR)/lib
