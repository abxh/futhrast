.PHONY: all run clean prepare

PROGNAME := futhrast-gui
BUILD_DIR := build

export PROGNAME

NOWARN_CFLAGS ?= -std=gnu11
CFLAGS ?= -std=gnu11 -Wall -Wextra -pedantic
CXXFLAGS ?= -std=c++17 -Wall -Wextra -pedantic

NOWARN_CFLAGS ?=  -O
CFLAGS += -O
CXXFLAGS += -O

# NOWARN_CFLAGS += -O3 -march=native -flto=auto -ffast-math
# CFLAGS += -O3 -march=native -flto=auto -ffast-math
# CXXFLAGS += -O3 -march=native -flto=auto -ffast-math

# enable ubsan sanitizer. may comment below chunk out.
# CFLAGS += -fsanitize=undefined
# CXXFLAGS += -fsanitize=undefined
# LDFLAGS += -fsanitize=undefined

LYS_BACKEND := opencl

export NOWARN_CFLAGS CFLAGS CXXFLAGS LDFLAGS LYS_BACKEND

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
