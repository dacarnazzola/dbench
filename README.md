Check CMakePresets.json for available presets; current options include:
    debug             - debugging build using gfortran
    gfortran-openblas - optimized build using gfortran, linking against OpenBLAS
    ifort-openblas    - optimized build using ifort, linking against OpenBLAS
    ifort-mkl         - optimized build using ifort, linking against Intel MKL
    ifx-openblas      - optimized build using ifx, linking against OpenBLAS
    ifx-mkl           - optimized build using ifx, linking against Intel MKL
    flang-openblas    - optimized build using Flang, linking against OpenBLAS

NOTE: Link-time optimization (-flto on gfortran/flang, -ipo on ifort/ifx) is disabled to prevent inlining benchmark functions from
      their original modules.

to create build/ folder:
    cmake --preset=<preset-name>

to build executables:
    cmake --build build

to add new dot product/matrix multiplication implementations, follow the interfaces defined below:
```
        pure subroutine dot_product_sp(a, b, c)
            import sp; implicit none
            real(sp), intent(in) :: a(:), b(size(a))
            real(sp), intent(out) :: c
        end subroutine

        pure subroutine dot_product_dp(a, b, c)
            import dp; implicit none
            real(dp), intent(in) :: a(:), b(size(a))
            real(dp), intent(out) :: c
        end subroutine

        pure subroutine matmul_sp(a, b, c)
            import sp; implicit none
            real(sp), intent(in) :: a(:,:), b(:,:)
            real(sp), intent(out) :: c(size(a,1),size(b,2))
        end subroutine

        pure subroutine matmul_dp(a, b, c)
            import dp; implicit none
            real(dp), intent(in) :: a(:,:), b(:,:)
            real(dp), intent(out) :: c(size(a,1),size(b,2))
        end subroutine
```
