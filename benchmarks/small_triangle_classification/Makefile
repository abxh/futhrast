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
	$(CUDA_ENV) futhark bench --backend=cuda 32pixels.fut
	$(CUDA_ENV) futhark bench --backend=cuda 64pixels.fut
	$(CUDA_ENV) futhark bench --backend=cuda 128pixels.fut
	$(CUDA_ENV) futhark bench --backend=cuda 256pixels.fut

clean:
	rm -f extract_obj_data *.in
	rm -f *.c 32pixels 64pixels 128pixels 256pixels
	rm -f *.fut.tuning
	rm -f *.json
	rm -rf *.prof
	rm -f *.md
