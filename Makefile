.PHONY: all run clean prepare

PROGNAME := futhrast-gui
BUILD_DIR := build

all: run

run: prepare
	$(MAKE) -C $(BUILD_DIR) PROGNAME=$(PROGNAME) \
		-f ../lib/github.com/diku-dk/lys/common.mk

clean:
	rm -rf $(BUILD_DIR)

prepare:
	@mkdir -p $(BUILD_DIR)
	@for f in *.fut; do \
		ln -sf ../$$f $(BUILD_DIR)/$$f; \
	done
	@ln -snf ../lib $(BUILD_DIR)/lib
