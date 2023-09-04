#!/bin/bash

OUTPUT_FILE="output.csv"
MAX_REPS=10000
N_MIN=1
N_MAX=100
N_STEP=1
DOT_PRODUCT_SP="F"
DOT_PRODUCT_DP="T"
MATMUL_SP="F"
MATMUL_DP="F"

export OMP_NUM_THREADS=1
export MKL_NUM_THREADS=1

rm -v $OUTPUT_FILE

rm -rf build/
cmake --preset=gfortran-openblas
cmake --build build
./bin/benchmark --output_file $OUTPUT_FILE --max_reps $MAX_REPS --n_min $N_MIN --n_max $N_MAX --n_step $N_STEP --dot_product_sp $DOT_PRODUCT_SP --dot_product_dp $DOT_PRODUCT_DP --matmul_sp $MATMUL_SP --matmul_dp $MATMUL_DP

rm -rf build/
cmake --preset=ifort-openblas
cmake --build build
./bin/benchmark --output_file $OUTPUT_FILE --max_reps $MAX_REPS --n_min $N_MIN --n_max $N_MAX --n_step $N_STEP --dot_product_sp $DOT_PRODUCT_SP --dot_product_dp $DOT_PRODUCT_DP --matmul_sp $MATMUL_SP --matmul_dp $MATMUL_DP

rm -rf build/
cmake --preset=ifx-openblas
cmake --build build
./bin/benchmark --output_file $OUTPUT_FILE --max_reps $MAX_REPS --n_min $N_MIN --n_max $N_MAX --n_step $N_STEP --dot_product_sp $DOT_PRODUCT_SP --dot_product_dp $DOT_PRODUCT_DP --matmul_sp $MATMUL_SP --matmul_dp $MATMUL_DP

rm -rf build/
cmake --preset=flang-openblas
cmake --build build
./bin/benchmark --output_file $OUTPUT_FILE --max_reps $MAX_REPS --n_min $N_MIN --n_max $N_MAX --n_step $N_STEP --dot_product_sp $DOT_PRODUCT_SP --dot_product_dp $DOT_PRODUCT_DP --matmul_sp $MATMUL_SP --matmul_dp $MATMUL_DP

#rm -rf build/
#cmake --preset=ifort-mkl
#cmake --build build
#./bin/benchmark --output_file $OUTPUT_FILE --max_reps $MAX_REPS --n_min $N_MIN --n_max $N_MAX --n_step $N_STEP --dot_product_sp $DOT_PRODUCT_SP --dot_product_dp $DOT_PRODUCT_DP --matmul_sp $MATMUL_SP --matmul_dp $MATMUL_DP
#
#rm -rf build/
#cmake --preset=ifx-mkl
#cmake --build build
#./bin/benchmark --output_file $OUTPUT_FILE --max_reps $MAX_REPS --n_min $N_MIN --n_max $N_MAX --n_step $N_STEP --dot_product_sp $DOT_PRODUCT_SP --dot_product_dp $DOT_PRODUCT_DP --matmul_sp $MATMUL_SP --matmul_dp $MATMUL_DP
