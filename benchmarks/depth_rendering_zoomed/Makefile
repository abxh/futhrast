HIP_ENV = C_INCLUDE_PATH=/opt/rocm/include:$${C_INCLUDE_PATH} \
          LIBRARY_PATH=/opt/rocm/lib:$${LIBRARY_PATH}

all: build run

build:
	g++ -std=c++20 \
		../tiny_obj_loader/tiny_obj_loader.cpp \
		extract_obj_data.cpp \
		-o extract_obj_data
	./extract_obj_data ../../models/bunny.obj
	./extract_obj_data ../../models/african_head.obj
	./extract_obj_data ../../models/monkey.obj
	./extract_obj_data ../../models/penger.obj
	./extract_obj_data ../../models/dragon.obj
	./extract_obj_data ../../models/lucy.obj
	./extract_obj_data ../../models/armadillo.obj
	rm -f extract_obj_data

define RUN_TEMPLATE
run_$(1):
	futhark bench --backend=multicore $(1).fut

	futhark bench --backend=opencl $(1).fut

	$(HIP_ENV) futhark bench --backend=hip $(1).fut

	rm -f $(1).c $(1) $(1).fut.tuning
endef

run: run_point run_line run_triangle_imm_scanline run_triangle_imm_barycentric run_triangle_tiled run_triangle_hybrid

run_point: $(eval $(call RUN_TEMPLATE,point))

run_line: $(eval $(call RUN_TEMPLATE,line))

run_triangle_imm_scanline: $(eval $(call RUN_TEMPLATE,triangle_imm_scanline))

run_triangle_imm_barycentric: $(eval $(call RUN_TEMPLATE,triangle_imm_barycentric))

run_triangle_tiled: $(eval $(call RUN_TEMPLATE,triangle_tiled))

run_triangle_hybrid: $(eval $(call RUN_TEMPLATE,triangle_hybrid))

clean:
	rm -f extract_obj_data *.in
	rm -f *.c point line triangle_imm_scanline triangle_imm_barycentric triangle_tiled triangle_hybrid
	rm -f *.fut.tuning
	rm -f *.json
	rm -rf *.prof
	rm -f *.md
