module user_dot_product_m
use, non_intrinsic :: constants_m, only: sp, dp
implicit none
private

    public :: do_dot_sp, do_dot_dp, &
              do_dot_unroll2_sp, do_dot_unroll2_dp, &
              do_dot_unroll3_sp, do_dot_unroll3_dp, &
              do_dot_unroll4_sp, do_dot_unroll4_dp, &
              do_dot_unroll5_sp, do_dot_unroll5_dp, &
              do_dot_unroll6_sp, do_dot_unroll6_dp, &
              do_dot_unroll7_sp, do_dot_unroll7_dp, &
              do_dot_unroll8_sp, do_dot_unroll8_dp

    contains


        pure subroutine do_dot_sp(a, b, c)
        !! simple do-loop implementation of dot product operation
            real(sp), intent(in) :: a(:), b(size(a))
            real(sp), intent(out) :: c
            integer :: i
            c = 0.0_sp
            do i=1,size(a)
                c = c + a(i)*b(i)
            end do
        end subroutine


        pure subroutine do_dot_unroll2_sp(a, b, c)
        !! do-loop dot product unrolled x2
            integer, parameter :: unroll_factor = 2
            real(sp), intent(in) :: a(:), b(size(a))
            real(sp), intent(out) :: c
            integer :: i, n, remainder
            real(sp) :: work(unroll_factor)
            n = size(a)
            remainder = mod(n, unroll_factor)
            c = 0.0_sp
            do i=1,remainder
                c = c + a(i)*b(i)
            end do
            work = 0.0_sp
            do i=remainder+1,n,unroll_factor
                work(1) = work(1) + a(i)*b(i)
                work(2) = work(2) + a(i+1)*b(i+1)
            end do
            do i=1,unroll_factor
                c = c + work(i)
            end do
        end subroutine


        pure subroutine do_dot_unroll3_sp(a, b, c)
        !! do-loop dot product unrolled x3
            integer, parameter :: unroll_factor = 3
            real(sp), intent(in) :: a(:), b(size(a))
            real(sp), intent(out) :: c
            integer :: i, n, remainder
            real(sp) :: work(unroll_factor)
            n = size(a)
            remainder = mod(n, unroll_factor)
            c = 0.0_sp
            do i=1,remainder
                c = c + a(i)*b(i)
            end do
            work = 0.0_sp
            do i=remainder+1,n,unroll_factor
                work(1) = work(1) + a(i)*b(i)
                work(2) = work(2) + a(i+1)*b(i+1)
                work(3) = work(3) + a(i+2)*b(i+2)
            end do
            do i=1,unroll_factor
                c = c + work(i)
            end do
        end subroutine


        pure subroutine do_dot_unroll4_sp(a, b, c)
        !! do-loop dot product unrolled x4
            integer, parameter :: unroll_factor = 4
            real(sp), intent(in) :: a(:), b(size(a))
            real(sp), intent(out) :: c
            integer :: i, n, remainder
            real(sp) :: work(unroll_factor)
            n = size(a)
            remainder = mod(n, unroll_factor)
            c = 0.0_sp
            do i=1,remainder
                c = c + a(i)*b(i)
            end do
            work = 0.0_sp
            do i=remainder+1,n,unroll_factor
                work(1) = work(1) + a(i)*b(i)
                work(2) = work(2) + a(i+1)*b(i+1)
                work(3) = work(3) + a(i+2)*b(i+2)
                work(4) = work(4) + a(i+3)*b(i+3)
            end do
            do i=1,unroll_factor
                c = c + work(i)
            end do
        end subroutine


        pure subroutine do_dot_unroll5_sp(a, b, c)
        !! do-loop dot product unrolled x5
            integer, parameter :: unroll_factor = 5
            real(sp), intent(in) :: a(:), b(size(a))
            real(sp), intent(out) :: c
            integer :: i, n, remainder
            real(sp) :: work(unroll_factor)
            n = size(a)
            remainder = mod(n, unroll_factor)
            c = 0.0_sp
            do i=1,remainder
                c = c + a(i)*b(i)
            end do
            work = 0.0_sp
            do i=remainder+1,n,unroll_factor
                work(1) = work(1) + a(i)*b(i)
                work(2) = work(2) + a(i+1)*b(i+1)
                work(3) = work(3) + a(i+2)*b(i+2)
                work(4) = work(4) + a(i+3)*b(i+3)
                work(5) = work(5) + a(i+4)*b(i+4)
            end do
            do i=1,unroll_factor
                c = c + work(i)
            end do
        end subroutine


        pure subroutine do_dot_unroll6_sp(a, b, c)
        !! do-loop dot product unrolled x6
            integer, parameter :: unroll_factor = 6
            real(sp), intent(in) :: a(:), b(size(a))
            real(sp), intent(out) :: c
            integer :: i, n, remainder
            real(sp) :: work(unroll_factor)
            n = size(a)
            remainder = mod(n, unroll_factor)
            c = 0.0_sp
            do i=1,remainder
                c = c + a(i)*b(i)
            end do
            work = 0.0_sp
            do i=remainder+1,n,unroll_factor
                work(1) = work(1) + a(i)*b(i)
                work(2) = work(2) + a(i+1)*b(i+1)
                work(3) = work(3) + a(i+2)*b(i+2)
                work(4) = work(4) + a(i+3)*b(i+3)
                work(5) = work(5) + a(i+4)*b(i+4)
                work(6) = work(6) + a(i+5)*b(i+5)
            end do
            do i=1,unroll_factor
                c = c + work(i)
            end do
        end subroutine


        pure subroutine do_dot_unroll7_sp(a, b, c)
        !! do-loop dot product unrolled x7
            integer, parameter :: unroll_factor = 7
            real(sp), intent(in) :: a(:), b(size(a))
            real(sp), intent(out) :: c
            integer :: i, n, remainder
            real(sp) :: work(unroll_factor)
            n = size(a)
            remainder = mod(n, unroll_factor)
            c = 0.0_sp
            do i=1,remainder
                c = c + a(i)*b(i)
            end do
            work = 0.0_sp
            do i=remainder+1,n,unroll_factor
                work(1) = work(1) + a(i)*b(i)
                work(2) = work(2) + a(i+1)*b(i+1)
                work(3) = work(3) + a(i+2)*b(i+2)
                work(4) = work(4) + a(i+3)*b(i+3)
                work(5) = work(5) + a(i+4)*b(i+4)
                work(6) = work(6) + a(i+5)*b(i+5)
                work(7) = work(7) + a(i+6)*b(i+6)
            end do
            do i=1,unroll_factor
                c = c + work(i)
            end do
        end subroutine


        pure subroutine do_dot_unroll8_sp(a, b, c)
        !! do-loop dot product unrolled x8
            integer, parameter :: unroll_factor = 8
            real(sp), intent(in) :: a(:), b(size(a))
            real(sp), intent(out) :: c
            integer :: i, n, remainder
            real(sp) :: work(unroll_factor)
            n = size(a)
            remainder = mod(n, unroll_factor)
            c = 0.0_sp
            do i=1,remainder
                c = c + a(i)*b(i)
            end do
            work = 0.0_sp
            do i=remainder+1,n,unroll_factor
                work(1) = work(1) + a(i)*b(i)
                work(2) = work(2) + a(i+1)*b(i+1)
                work(3) = work(3) + a(i+2)*b(i+2)
                work(4) = work(4) + a(i+3)*b(i+3)
                work(5) = work(5) + a(i+4)*b(i+4)
                work(6) = work(6) + a(i+5)*b(i+5)
                work(7) = work(7) + a(i+6)*b(i+6)
                work(8) = work(8) + a(i+7)*b(i+7)
            end do
            do i=1,unroll_factor
                c = c + work(i)
            end do
        end subroutine


        pure subroutine do_dot_dp(a, b, c)
        !! simple do-loop implementation of dot product operation
            real(dp), intent(in) :: a(:), b(size(a))
            real(dp), intent(out) :: c
            integer :: i
            c = 0.0_dp
            do i=1,size(a)
                c = c + a(i)*b(i)
            end do
        end subroutine


        pure subroutine do_dot_unroll2_dp(a, b, c)
        !! do-loop dot product unrolled x2
            integer, parameter :: unroll_factor = 2
            real(dp), intent(in) :: a(:), b(size(a))
            real(dp), intent(out) :: c
            integer :: i, n, remainder
            real(dp) :: work(unroll_factor)
            n = size(a)
            remainder = mod(n, unroll_factor)
            c = 0.0_dp
            do i=1,remainder
                c = c + a(i)*b(i)
            end do
            work = 0.0_dp
            do i=remainder+1,n,unroll_factor
                work(1) = work(1) + a(i)*b(i)
                work(2) = work(2) + a(i+1)*b(i+1)
            end do
            do i=1,unroll_factor
                c = c + work(i)
            end do
        end subroutine


        pure subroutine do_dot_unroll3_dp(a, b, c)
        !! do-loop dot product unrolled x3
            integer, parameter :: unroll_factor = 3
            real(dp), intent(in) :: a(:), b(size(a))
            real(dp), intent(out) :: c
            integer :: i, n, remainder
            real(dp) :: work(unroll_factor)
            n = size(a)
            remainder = mod(n, unroll_factor)
            c = 0.0_dp
            do i=1,remainder
                c = c + a(i)*b(i)
            end do
            work = 0.0_dp
            do i=remainder+1,n,unroll_factor
                work(1) = work(1) + a(i)*b(i)
                work(2) = work(2) + a(i+1)*b(i+1)
                work(3) = work(3) + a(i+2)*b(i+2)
            end do
            do i=1,unroll_factor
                c = c + work(i)
            end do
        end subroutine


        pure subroutine do_dot_unroll4_dp(a, b, c)
        !! do-loop dot product unrolled x4
            integer, parameter :: unroll_factor = 4
            real(dp), intent(in) :: a(:), b(size(a))
            real(dp), intent(out) :: c
            integer :: i, n, remainder
            real(dp) :: work(unroll_factor)
            n = size(a)
            remainder = mod(n, unroll_factor)
            c = 0.0_dp
            do i=1,remainder
                c = c + a(i)*b(i)
            end do
            work = 0.0_dp
            do i=remainder+1,n,unroll_factor
                work(1) = work(1) + a(i)*b(i)
                work(2) = work(2) + a(i+1)*b(i+1)
                work(3) = work(3) + a(i+2)*b(i+2)
                work(4) = work(4) + a(i+3)*b(i+3)
            end do
            do i=1,unroll_factor
                c = c + work(i)
            end do
        end subroutine


        pure subroutine do_dot_unroll5_dp(a, b, c)
        !! do-loop dot product unrolled x5
            integer, parameter :: unroll_factor = 5
            real(dp), intent(in) :: a(:), b(size(a))
            real(dp), intent(out) :: c
            integer :: i, n, remainder
            real(dp) :: work(unroll_factor)
            n = size(a)
            remainder = mod(n, unroll_factor)
            c = 0.0_dp
            do i=1,remainder
                c = c + a(i)*b(i)
            end do
            work = 0.0_dp
            do i=remainder+1,n,unroll_factor
                work(1) = work(1) + a(i)*b(i)
                work(2) = work(2) + a(i+1)*b(i+1)
                work(3) = work(3) + a(i+2)*b(i+2)
                work(4) = work(4) + a(i+3)*b(i+3)
                work(5) = work(5) + a(i+4)*b(i+4)
            end do
            do i=1,unroll_factor
                c = c + work(i)
            end do
        end subroutine


        pure subroutine do_dot_unroll6_dp(a, b, c)
        !! do-loop dot product unrolled x6
            integer, parameter :: unroll_factor = 6
            real(dp), intent(in) :: a(:), b(size(a))
            real(dp), intent(out) :: c
            integer :: i, n, remainder
            real(dp) :: work(unroll_factor)
            n = size(a)
            remainder = mod(n, unroll_factor)
            c = 0.0_dp
            do i=1,remainder
                c = c + a(i)*b(i)
            end do
            work = 0.0_dp
            do i=remainder+1,n,unroll_factor
                work(1) = work(1) + a(i)*b(i)
                work(2) = work(2) + a(i+1)*b(i+1)
                work(3) = work(3) + a(i+2)*b(i+2)
                work(4) = work(4) + a(i+3)*b(i+3)
                work(5) = work(5) + a(i+4)*b(i+4)
                work(6) = work(6) + a(i+5)*b(i+5)
            end do
            do i=1,unroll_factor
                c = c + work(i)
            end do
        end subroutine


        pure subroutine do_dot_unroll7_dp(a, b, c)
        !! do-loop dot product unrolled x7
            integer, parameter :: unroll_factor = 7
            real(dp), intent(in) :: a(:), b(size(a))
            real(dp), intent(out) :: c
            integer :: i, n, remainder
            real(dp) :: work(unroll_factor)
            n = size(a)
            remainder = mod(n, unroll_factor)
            c = 0.0_dp
            do i=1,remainder
                c = c + a(i)*b(i)
            end do
            work = 0.0_dp
            do i=remainder+1,n,unroll_factor
                work(1) = work(1) + a(i)*b(i)
                work(2) = work(2) + a(i+1)*b(i+1)
                work(3) = work(3) + a(i+2)*b(i+2)
                work(4) = work(4) + a(i+3)*b(i+3)
                work(5) = work(5) + a(i+4)*b(i+4)
                work(6) = work(6) + a(i+5)*b(i+5)
                work(7) = work(7) + a(i+6)*b(i+6)
            end do
            do i=1,unroll_factor
                c = c + work(i)
            end do
        end subroutine


        pure subroutine do_dot_unroll8_dp(a, b, c)
        !! do-loop dot product unrolled x8
            integer, parameter :: unroll_factor = 8
            real(dp), intent(in) :: a(:), b(size(a))
            real(dp), intent(out) :: c
            integer :: i, n, remainder
            real(dp) :: work(unroll_factor)
            n = size(a)
            remainder = mod(n, unroll_factor)
            c = 0.0_dp
            do i=1,remainder
                c = c + a(i)*b(i)
            end do
            work = 0.0_dp
            do i=remainder+1,n,unroll_factor
                work(1) = work(1) + a(i)*b(i)
                work(2) = work(2) + a(i+1)*b(i+1)
                work(3) = work(3) + a(i+2)*b(i+2)
                work(4) = work(4) + a(i+3)*b(i+3)
                work(5) = work(5) + a(i+4)*b(i+4)
                work(6) = work(6) + a(i+5)*b(i+5)
                work(7) = work(7) + a(i+6)*b(i+6)
                work(8) = work(8) + a(i+7)*b(i+7)
            end do
            do i=1,unroll_factor
                c = c + work(i)
            end do
        end subroutine


end module user_dot_product_m
