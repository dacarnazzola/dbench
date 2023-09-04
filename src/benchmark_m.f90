module benchmark_m
implicit none
private

    public :: benchmark_all

    contains

        impure subroutine benchmark_all(r_max, fid)
            integer, intent(in) :: r_max, fid
            integer :: temp
            temp = r_max + fid
        end subroutine

end module benchmark_m
