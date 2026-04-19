HIP_ENV = C_INCLUDE_PATH=/opt/rocm/include:$${C_INCLUDE_PATH} \
          LIBRARY_PATH=/opt/rocm/lib:$${LIBRARY_PATH}

all: build run

build:
	g++ -std=c++20 \
		../lib/github.com/abxh/lys/utils/tiny_obj_loader/tiny_obj_loader.cpp \
		extract_obj_data.cpp \
		-o extract_obj_data
	./extract_obj_data ../bunny.obj
	./extract_obj_data ../african_head.obj
	./extract_obj_data ../monkey.obj
	./extract_obj_data ../penger.obj
	rm -f extract_obj_data

define RUN_TEMPLATE
run_$(1):
	futhark bench --backend=multicore $(1).fut

	futhark bench --backend=opencl $(1).fut

	$(HIP_ENV) futhark bench --backend=hip $(1).fut

	rm -f $(1).c $(1) $(1).fut.tuning
endef

run: run_point run_line run_triangle_imm run_triangle_tiled

run_point: $(eval $(call RUN_TEMPLATE,point))

run_line: $(eval $(call RUN_TEMPLATE,line))

run_triangle_imm: $(eval $(call RUN_TEMPLATE,triangle_imm))

run_triangle_tiled: $(eval $(call RUN_TEMPLATE,triangle_tiled))

clean:
	rm -f extract_obj_data *.in
	rm -f *.c point line triangle_imm triangle_tiled
	rm -f *.fut.tuning
	rm -f *.json
	rm -rf *.prof
