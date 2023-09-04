module language_intrinsic_functions_m
use, non_intrinsic :: constants_m, only: sp, dp
implicit none
private

    public :: intrinsic_dot_product_sp, &
              intrinsic_dot_product_dp, &
              intrinsic_matmul_sp, &
              intrinsic_matmul_dp

    contains

        pure subroutine intrinsic_dot_product_sp(a, b, c)
            real(sp), intent(in) :: a(:), b(size(a))
            real(sp), intent(out) :: c
            c = dot_product(a, b)
        end subroutine

        pure subroutine intrinsic_dot_product_dp(a, b, c)
            real(dp), intent(in) :: a(:), b(size(a))
            real(dp), intent(out) :: c
            c = dot_product(a, b)
        end subroutine

        pure subroutine intrinsic_matmul_sp(a, b, c)
            real(sp), intent(in) :: a(:,:), b(:,:)
            real(sp), intent(out) :: c(size(a,1),size(b,2))
            c = matmul(a, b)
        end subroutine

        pure subroutine intrinsic_matmul_dp(a, b, c)
            real(dp), intent(in) :: a(:,:), b(:,:)
            real(dp), intent(out) :: c(size(a,1),size(b,2))
            c = matmul(a, b)
        end subroutine

end module language_intrinsic_functions_m
