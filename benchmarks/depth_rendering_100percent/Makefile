HIP_ENV = HIP_PATH=/opt/rocm C_INCLUDE_PATH=/opt/rocm/include LIBRARY_PATH=/opt/rocm/lib
CUDA_ENV = CUDA_PATH=/opt/cuda/ C_INCLUDE_PATH=/opt/cuda/include LIBRARY_PATH=/opt/cuda/targets/x86_64-linux/lib/ 

all: build run

build:
	g++ -std=c++20 \
		../tiny_obj_loader/tiny_obj_loader.cpp \
		../extract_obj_data.cpp \
		-o extract_obj_data
	./extract_obj_data ../../models/bunny.obj
	./extract_obj_data ../../models/penger.obj
	./extract_obj_data ../../models/monkey.obj
	./extract_obj_data ../../models/dragon.obj
	./extract_obj_data ../../models/lucy.obj
	./extract_obj_data ../../models/armadillo.obj
	rm -f extract_obj_data

# futhark bench --backend=multicore $(1).fut
#$(HIP_ENV) futhark bench --backend=hip $(1).fut

define RUN_TEMPLATE
run_$(1):
	$(CUDA_ENV) futhark bench --backend=cuda $(1).fut --profile --json=$(1).json
	futhark profile $(1).json
	rm -f $(1).c $(1) $(1).fut.tuning
endef

run: run_triangle_imm_scanline run_triangle_imm_pineda run_triangle_tiled_pineda run_triangle_hybrid_pineda

run_triangle_imm_scanline: $(eval $(call RUN_TEMPLATE,triangle_imm_scanline))

run_triangle_imm_pineda: $(eval $(call RUN_TEMPLATE,triangle_imm_pineda))

run_triangle_tiled_pineda: $(eval $(call RUN_TEMPLATE,triangle_tiled_pineda))

run_triangle_hybrid_pineda: $(eval $(call RUN_TEMPLATE,triangle_hybrid_pineda))

clean:
	rm -f extract_obj_data *.in
	rm -f *.c triangle_imm_scanline triangle_imm_pineda triangle_tiled_pineda triangle_hybrid_pineda
	rm -f *.fut.tuning
	rm -f *.json
	rm -rf *.prof
	rm -f *.md
