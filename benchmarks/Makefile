
all: build run

build:
	g++ -std=c++20 setup.cpp ../lib/github.com/abxh/lys/file_parsers/obj/tiny_obj_loader.cpp -o extract_obj_data
	./extract_obj_data ../bunny.obj
	./extract_obj_data ../african_head.obj
	./extract_obj_data ../monkey.obj
	./extract_obj_data ../penger.obj
	rm extract_obj_data

run: run-point run-line run-triangle-imm run-triangle-tiled run-triangle-tiled-segmented
	
run-point:
	futhark bench --backend=multicore point.fut
	
	futhark bench --backend=opencl point.fut
	
	C_INCLUDE_PATH=/opt/rocm/include:$(C_INCLUDE_PATH) \
	LIBRARY_PATH=/opt/rocm/lib:$(LIBRARY_PATH) \
	futhark bench --backend=hip point.fut
	
	rm point.c point

run-line:
	futhark bench --backend=multicore line.fut
	
	futhark bench --backend=opencl line.fut
	
	C_INCLUDE_PATH=/opt/rocm/include:$(C_INCLUDE_PATH) \
	LIBRARY_PATH=/opt/rocm/lib:$(LIBRARY_PATH) \
	futhark bench --backend=hip line.fut
	
	rm line.c line

run-triangle-imm:
	futhark bench --backend=multicore triangle_imm.fut
	
	futhark bench --backend=opencl triangle_imm.fut
	
	C_INCLUDE_PATH=/opt/rocm/include:$(C_INCLUDE_PATH) \
	LIBRARY_PATH=/opt/rocm/lib:$(LIBRARY_PATH) \
	futhark bench --backend=hip triangle_imm.fut
	
	rm triangle_imm.c triangle_imm

run-triangle-tiled:
	futhark bench --backend=multicore triangle_tiled.fut
	
	futhark autotune --backend=opencl triangle_tiled.fut
	futhark bench --backend=opencl triangle_tiled.fut
	
	C_INCLUDE_PATH=/opt/rocm/include:$(C_INCLUDE_PATH) \
	LIBRARY_PATH=/opt/rocm/lib:$(LIBRARY_PATH) \
	futhark autotune --backend=hip triangle_tiled.fut
	C_INCLUDE_PATH=/opt/rocm/include:$(C_INCLUDE_PATH) \
	LIBRARY_PATH=/opt/rocm/lib:$(LIBRARY_PATH) \
	futhark bench --backend=hip triangle_tiled.fut
	
	rm triangle_tiled.c triangle_tiled triangle_tiled.fut.tuning

run-triangle-tiled-segmented:
	futhark bench --backend=multicore triangle_tiled_segmented.fut
	
	futhark autotune --backend=opencl triangle_tiled_segmented.fut
	futhark bench --backend=opencl triangle_tiled_segmented.fut
	
	C_INCLUDE_PATH=/opt/rocm/include:$(C_INCLUDE_PATH) \
	LIBRARY_PATH=/opt/rocm/lib:$(LIBRARY_PATH) \
	futhark autotune --backend=hip triangle_tiled_segmented.fut
	C_INCLUDE_PATH=/opt/rocm/include:$(C_INCLUDE_PATH) \
	LIBRARY_PATH=/opt/rocm/lib:$(LIBRARY_PATH) \
	futhark bench --backend=hip triangle_tiled_segmented.fut
	
	rm triangle_tiled_segmented.c triangle_tiled_segmented triangle_tiled_segmented.fut.tuning

clean:
	rm -f extract_obj_data
	rm -f *.in
	rm -f point.c point point.fut.tuning
	rm -f line.c line line.fut.tuning
	rm -f triangle_imm.c triangle_imm triangle_imm.fut.tuning
	rm -f triangle_tiled.c triangle_tiled triangle_tiled.fut.tuning
	rm -f triangle_tiled_segmented.c triangle_tiled_segmented triangle_tiled_segmented.fut.tuning
