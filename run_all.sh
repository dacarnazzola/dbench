#!/bin/bash

export OUTPUT_FILE="output.csv"
export MAX_REPS=20
export N_MIN=8
export N_MAX=256
export N_STEP=8

export OMP_NUM_THREADS=1
export MKL_NUM_THREADS=1

rm -v $OUTPUT_FILE

rm -rf build/
cmake --preset=release-gfortran-openblas
cmake --build build
./bin/benchmark --output_file $OUTPUT_FILE --max_reps $MAX_REPS --n_min $N_MIN --n_max $N_MAX --n_step $N_STEP

rm -rf build/
cmake --preset=release-ifort-openblas
cmake --build build
./bin/benchmark --output_file $OUTPUT_FILE --max_reps $MAX_REPS --n_min $N_MIN --n_max $N_MAX --n_step $N_STEP

rm -rf build/
cmake --preset=release-ifort-mkl
cmake --build build
./bin/benchmark --output_file $OUTPUT_FILE --max_reps $MAX_REPS --n_min $N_MIN --n_max $N_MAX --n_step $N_STEP

rm -rf build/
cmake --preset=release-ifx-openblas
cmake --build build
./bin/benchmark --output_file $OUTPUT_FILE --max_reps $MAX_REPS --n_min $N_MIN --n_max $N_MAX --n_step $N_STEP

rm -rf build/
cmake --preset=release-ifx-mkl
cmake --build build
./bin/benchmark --output_file $OUTPUT_FILE --max_reps $MAX_REPS --n_min $N_MIN --n_max $N_MAX --n_step $N_STEP

rm -rf build/
cmake --preset=release-flang-openblas
cmake --build build
./bin/benchmark --output_file $OUTPUT_FILE --max_reps $MAX_REPS --n_min $N_MIN --n_max $N_MAX --n_step $N_STEP
