module external_blas_m
use, non_intrinsic :: constants_m, only: sp, dp
implicit none
private

    public :: external_sdot, external_ddot, &
              external_sgemm, external_dgemm

    interface

        pure real(sp) function sdot(n, a, ia, b, ib)
            import sp; implicit none
            integer, intent(in) :: n, ia, ib
            real(sp), intent(in) :: a(*), b(*)
        end function

        pure real(dp) function ddot(n, a, ia, b, ib)
            import dp; implicit none
            integer, intent(in) :: n, ia, ib
            real(dp), intent(in) :: a(*), b(*)
        end function

        pure subroutine sgemm(ta, tb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
            import sp; implicit none
            character, intent(in) :: ta, tb
            integer, intent(in) :: m, n, k, lda, ldb, ldc
            real(sp), intent(in) :: alpha, beta, a(lda,*), b(ldb,*)
            real(sp), intent(out) :: c(ldc,*)
        end subroutine

        pure subroutine dgemm(ta, tb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
            import dp; implicit none
            character, intent(in) :: ta, tb
            integer, intent(in) :: m, n, k, lda, ldb, ldc
            real(dp), intent(in) :: alpha, beta, a(lda,*), b(ldb,*)
            real(dp), intent(out) :: c(ldc,*)
        end subroutine

    end interface

    contains

        pure subroutine external_sdot(a, b, c)
            real(sp), intent(in) :: a(:), b(size(a))
            real(sp), intent(out) :: c
            c = sdot(size(a), a, 1, b, 1)
        end subroutine

        pure subroutine external_ddot(a, b, c)
            real(dp), intent(in) :: a(:), b(size(a))
            real(dp), intent(out) :: c
            c = ddot(size(a), a, 1, b, 1)
        end subroutine

        pure subroutine external_sgemm(a, b, c)
            real(sp), intent(in) :: a(:,:), b(:,:)
            real(sp), intent(out) :: c(size(a,1),size(b,2))
            call sgemm('n', 'n', size(a,1), size(b,2), size(a,2), 1.0_sp, a, size(a,1), b, size(b,1), 0.0_sp, c, size(c,1))
        end subroutine

        pure subroutine external_dgemm(a, b, c)
            real(dp), intent(in) :: a(:,:), b(:,:)
            real(dp), intent(out) :: c(size(a,1),size(b,2))
            call dgemm('n', 'n', size(a,1), size(b,2), size(a,2), 1.0_dp, a, size(a,1), b, size(b,1), 0.0_dp, c, size(c,1))
        end subroutine

end module external_blas_m
