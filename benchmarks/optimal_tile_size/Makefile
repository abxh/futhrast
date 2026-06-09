HIP_ENV = HIP_PATH=/opt/rocm C_INCLUDE_PATH=/opt/rocm/include LIBRARY_PATH=/opt/rocm/lib
CUDA_ENV = CUDA_PATH=/opt/cuda/ C_INCLUDE_PATH=/opt/cuda/include LIBRARY_PATH=/opt/cuda/targets/x86_64-linux/lib/ 

all: build run

build:
	g++ -std=c++20 \
		../tiny_obj_loader/tiny_obj_loader.cpp \
		extract_obj_data.cpp \
		-o extract_obj_data
	./extract_obj_data ../../models/dragon.obj
	rm -f extract_obj_data

run:
	$(CUDA_ENV) futhark bench --backend=cuda test_16x16_4x4.fut
	$(CUDA_ENV) futhark bench --backend=cuda test_32x32_4x4.fut
	$(CUDA_ENV) futhark bench --backend=cuda test_32x32_8x8.fut
	$(CUDA_ENV) futhark bench --backend=cuda test_64x64_8x8.fut
	$(CUDA_ENV) futhark bench --backend=cuda test_64x64_16x16.fut
	$(CUDA_ENV) futhark bench --backend=cuda test_64x64_4x4.fut
	$(CUDA_ENV) futhark bench --backend=cuda test_128x128_8x8.fut
	$(CUDA_ENV) futhark bench --backend=cuda test_128x128_16x16.fut

clean:
	rm -f extract_obj_data *.in
	rm -f *.c test_16x16_4x4 test_32x32_4x4 test_32x32_8x8 test_64x64_4x4 test_64x64_8x8 test_64x64_16x16 test_128x128_8x8 test_128x128_16x16
	rm -f *.fut.tuning
	rm -f *.json
	rm -rf *.prof
	rm -f *.md
