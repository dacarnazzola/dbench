# explicitly name all files in src/ directory
set(MODULE_FILES 
    constants_m.f90
    language_intrinsic_functions_m.f90
    external_blas_m.f90
    benchmark_m.f90
   )

# compile BENCHMARK executable
add_executable(benchmark ${MODULE_FILES} main.f90)
