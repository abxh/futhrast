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

run: 
	$(CUDA_ENV) futhark bench --backend=cuda linear_linear.fut
	$(CUDA_ENV) futhark bench --backend=cuda linear_diagonal.fut
	$(CUDA_ENV) futhark bench --backend=cuda linear_xshfoff.fut
	$(CUDA_ENV) futhark bench --backend=cuda linear_morton.fut
	$(CUDA_ENV) futhark bench --backend=cuda diagonal_linear.fut
	$(CUDA_ENV) futhark bench --backend=cuda diagonal_diagonal.fut
	$(CUDA_ENV) futhark bench --backend=cuda diagonal_xshfoff.fut
	$(CUDA_ENV) futhark bench --backend=cuda diagonal_morton.fut
	$(CUDA_ENV) futhark bench --backend=cuda xshfoff_linear.fut
	$(CUDA_ENV) futhark bench --backend=cuda xshfoff_diagonal.fut
	$(CUDA_ENV) futhark bench --backend=cuda xshfoff_xshfoff.fut
	$(CUDA_ENV) futhark bench --backend=cuda xshfoff_morton.fut
	$(CUDA_ENV) futhark bench --backend=cuda morton_linear.fut
	$(CUDA_ENV) futhark bench --backend=cuda morton_diagonal.fut
	$(CUDA_ENV) futhark bench --backend=cuda morton_xshfoff.fut
	$(CUDA_ENV) futhark bench --backend=cuda morton_morton.fut

clean:
	rm -f extract_obj_data *.in
	rm -f *.c linear_linear linear_diagonal linear_xshfoff linear_morton
	rm -f *.c diagonal_linear diagonal_diagonal diagonal_xshfoff diagonal_morton
	rm -f *.c xshfoff_linear xshfoff_diagonal xshfoff_xshfoff xshfoff_morton
	rm -f *.c morton_linear morton_diagonal morton_xshfoff morton_morton
	rm -f *.fut.tuning
	rm -f *.json
	rm -rf *.prof
	rm -f *.md
