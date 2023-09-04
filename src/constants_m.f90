module constants_m
use, intrinsic :: iso_fortran_env, only: i64 => int64, &
                                         sp => real32, &
                                         dp => real64, &
                                         stdout => output_unit, &
                                         fc_ver => compiler_version, &
                                         fc_opt => compiler_options
implicit none
private

    public :: i64, sp, dp, stdout, fc_ver, fc_opt

end module constants_m
