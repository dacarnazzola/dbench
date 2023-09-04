Check CMakePresets.json for available presets; current options include:
    debug                     - debugging build using gfortran
    release-gfortran-openblas - optimized build using gfortran, linking against OpenBLAS

to create build/ folder:
    cmake --preset=<preset-name>

to build executables:
    cmake --build build
