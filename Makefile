.PHONY: all run build prepare clean

PROGNAME := futhrast-gui
BUILD_DIR := build

export PROGNAME

# NOWARN_CFLAGS ?= -std=gnu11
# CFLAGS ?= -std=gnu11 -Wall -Wextra -pedantic
# CXXFLAGS ?= -std=c++17 -Wall -Wextra -pedantic

NOWARN_CFLAGS ?= -O
CFLAGS += -O
CXXFLAGS += -O 

# NOWARN_CFLAGS += -I/opt/rocm/include -L/opt/rocm/lib
# CFLAGS += -I/opt/rocm/include -L/opt/rocm/lib
# CXXFLAGS += -I/opt/rocm/include -L/opt/rocm/lib

# NOWARN_CFLAGS ?= -O3 -march=native -flto=auto -ffast-math
# CFLAGS += -O3 -march=native -flto=auto -ffast-math
# CXXFLAGS += -O3 -march=native -flto=auto -ffast-math

# enable ubsan sanitizer. may comment below chunk out.
# CFLAGS += -fsanitize=undefined
# CXXFLAGS += -fsanitize=undefined
# LDFLAGS += -fsanitize=undefined

LYS_BACKEND := opencl
LYS_FRONTEND := sdl

export NOWARN_CFLAGS CFLAGS CXXFLAGS LDFLAGS LYS_BACKEND LYS_FRONTEND

all: build

run: build
	$(MAKE) run \
		-C $(BUILD_DIR) \
		-f ../lib/github.com/abxh/lys/common.mk

build: prepare
	$(MAKE) \
		-C $(BUILD_DIR) \
		-f ../lib/github.com/abxh/lys/common.mk

clean:
	rm -rf build

prepare: 
	@mkdir -p $(BUILD_DIR)
	@for f in *.fut *.bin *.obj; do \
		ln -sf "../$$f" "$(BUILD_DIR)/$$f"; \
	done
	@ln -snf ../lib $(BUILD_DIR)/lib
