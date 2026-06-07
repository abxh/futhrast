HIP_ENV = HIP_PATH=/opt/rocm C_INCLUDE_PATH=/opt/rocm/include LIBRARY_PATH=/opt/rocm/lib
CUDA_ENV = C_INCLUDE_PATH=/opt/cuda/include LIBRARY_PATH=/opt/cuda/lib

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
	$(HIP_ENV) futhark bench --backend=hip test_custom.fut

clean:
	rm -f extract_obj_data *.in
	rm -f *.c test_custom
	rm -f *.fut.tuning
	rm -f *.json
	rm -rf *.prof
	rm -f *.md
