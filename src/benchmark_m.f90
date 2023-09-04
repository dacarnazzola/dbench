module benchmark_m
use, non_intrinsic :: constants_m, only: i64, sp, dp, stdout, fc_ver, fc_opt
use, non_intrinsic :: language_intrinsic_functions_m, only: intrinsic_dot_product_sp, intrinsic_dot_product_dp, &
                                                            intrinsic_matmul_sp, intrinsic_matmul_dp
use, non_intrinsic :: external_blas_m, only: external_sdot, external_ddot, external_sgemm, external_dgemm
use, non_intrinsic :: user_dot_product_m, only: do_dot_sp, do_dot_dp, do_dot_unroll2_sp, do_dot_unroll2_dp, do_dot_unroll3_sp, &
                                                do_dot_unroll3_dp, do_dot_unroll4_sp, do_dot_unroll4_dp, do_dot_unroll5_sp, &
                                                do_dot_unroll5_dp, do_dot_unroll6_sp, do_dot_unroll6_dp, do_dot_unroll7_sp, &
                                                do_dot_unroll7_dp, do_dot_unroll8_sp, do_dot_unroll8_dp
implicit none
private

    public :: benchmark_config, echo_config, benchmark_all

    type :: benchmark_config
        integer :: max_reps = 10
        integer :: output_fid = stdout
        integer :: n_min = 8
        integer :: n_max = 256
        integer :: n_step = 8
        character :: delimiter = '|'
    end type

    interface

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

    end interface

    contains

        impure elemental subroutine echo_config(config)
            type(benchmark_config), intent(in) :: config
            write(stdout,'(a,i0)') 'OUTPUT_FID: ',config%output_fid
            write(stdout,'(a,i0)') 'MAX_REPS: ',config%max_reps
            write(stdout,'(a,i0)') 'N_MIN: ',config%n_min
            write(stdout,'(a,i0)') 'N_MAX: ',config%n_max
            write(stdout,'(a,i0)') 'N_STEP: ',config%n_step
            write(stdout,'(a)') 'N_STEP: '//config%delimiter
        end subroutine echo_config

        impure subroutine benchmark_all(config)
            type(benchmark_config), intent(in) :: config

            write(stdout,'(a)') 'Beginning benchmark ... Single-Precision (real32) DOT PRODUCT'
            call benchmark_one_dot_product_sp(intrinsic_dot_product_sp,         'intrinsic dot_product sp', config)
            call benchmark_one_dot_product_sp(           external_sdot,                    'external sdot', config)
            call benchmark_one_dot_product_sp(               do_dot_sp,           'do-loop dot product sp', config)
            call benchmark_one_dot_product_sp(       do_dot_unroll2_sp,'do-loop dot product urolled x2 sp', config)
            call benchmark_one_dot_product_sp(       do_dot_unroll3_sp,'do-loop dot product urolled x3 sp', config)
            call benchmark_one_dot_product_sp(       do_dot_unroll4_sp,'do-loop dot product urolled x4 sp', config)
            call benchmark_one_dot_product_sp(       do_dot_unroll5_sp,'do-loop dot product urolled x5 sp', config)
            call benchmark_one_dot_product_sp(       do_dot_unroll6_sp,'do-loop dot product urolled x6 sp', config)
            call benchmark_one_dot_product_sp(       do_dot_unroll7_sp,'do-loop dot product urolled x7 sp', config)
            call benchmark_one_dot_product_sp(       do_dot_unroll8_sp,'do-loop dot product urolled x8 sp', config)

            write(stdout,'(a)') 'Beginning benchmark ... Double-Precision (real64) DOT PRODUCT'
            call benchmark_one_dot_product_dp(intrinsic_dot_product_dp,         'intrinsic dot_product dp', config)
            call benchmark_one_dot_product_dp(           external_ddot,                    'external ddot', config)
            call benchmark_one_dot_product_dp(               do_dot_dp,           'do-loop dot product dp', config)
            call benchmark_one_dot_product_dp(       do_dot_unroll2_dp,'do-loop dot product urolled x2 dp', config)
            call benchmark_one_dot_product_dp(       do_dot_unroll3_dp,'do-loop dot product urolled x3 dp', config)
            call benchmark_one_dot_product_dp(       do_dot_unroll4_dp,'do-loop dot product urolled x4 dp', config)
            call benchmark_one_dot_product_dp(       do_dot_unroll5_dp,'do-loop dot product urolled x5 dp', config)
            call benchmark_one_dot_product_dp(       do_dot_unroll6_dp,'do-loop dot product urolled x6 dp', config)
            call benchmark_one_dot_product_dp(       do_dot_unroll7_dp,'do-loop dot product urolled x7 dp', config)
            call benchmark_one_dot_product_dp(       do_dot_unroll8_dp,'do-loop dot product urolled x8 dp', config)

            write(stdout,'(a)') 'Beginning benchmark ... Single-Precision (real32) MATRIX MULTIPLICATION'
            call benchmark_one_matmul_sp(intrinsic_matmul_sp, 'intrinsic matmul sp', config)
            call benchmark_one_matmul_sp(     external_sgemm,      'external sgemm', config)

            write(stdout,'(a)') 'Beginning benchmark ... Double-Precision (real64) MATRIX MULTIPLICATION'
            call benchmark_one_matmul_dp(intrinsic_matmul_dp, 'intrinsic matmul dp', config)
            call benchmark_one_matmul_dp(     external_dgemm,      'external dgemm', config)

        end subroutine benchmark_all


        impure subroutine benchmark_one_dot_product_sp(test, label, config)
            procedure(dot_product_sp) :: test
            character(len=*), intent(in) :: label
            type(benchmark_config), intent(in) :: config
            real(sp), allocatable :: a(:), b(:)
            real(sp) :: c(config%max_reps)
            integer(i64) :: count_rate, count1(config%max_reps), count2(config%max_reps)
            integer :: r, n
            real(dp) :: elapsed(config%max_reps), gflops(config%max_reps), avg_elapsed, avg_gflops
            size_loop: do n=config%n_min,config%n_max,config%n_step
                if (allocated(a)) deallocate(a)
                if (allocated(b)) deallocate(b)
                allocate(a(n), b(n))
                call system_clock(count_rate=count_rate)
                test_loop: do r=1,config%max_reps
                    call random_number(a)
                    call random_number(b)
                    call system_clock(count=count1(r))
                    call test(a, b, c(r))
                    call system_clock(count=count2(r))
                end do test_loop
                elapsed = real(max(count2 - count1, 1_i64), dp)/real(count_rate, dp)
                gflops = 2.0_dp*real(n, dp)/1.0D+9/elapsed
                avg_elapsed = sum(elapsed)/real(config%max_reps, dp)
                avg_gflops = sum(gflops)/real(config%max_reps, dp)
                write(stdout,'(a48,a,i0,a,i0,a,f0.2,a,e13.6,a)') label,' completed ',config%max_reps,' reps of size ',n, &
                                                                 ' with average ',avg_gflops,' GFLOPS (',maxval(c),')'
                flush(stdout)
                write(config%output_fid,'(6a,i0,2(a,e13.6))') trim(adjustl(fc_ver())),config%delimiter, &
                                                              trim(adjustl(fc_opt())),config%delimiter, &
                                                              label,config%delimiter, &
                                                              n,config%delimiter, &
                                                              avg_elapsed,config%delimiter, &
                                                              avg_gflops
                flush(config%output_fid)
            end do size_loop
        end subroutine benchmark_one_dot_product_sp


        impure subroutine benchmark_one_dot_product_dp(test, label, config)
            procedure(dot_product_dp) :: test
            character(len=*), intent(in) :: label
            type(benchmark_config), intent(in) :: config
            real(dp), allocatable :: a(:), b(:)
            real(dp) :: c(config%max_reps)
            integer(i64) :: count_rate, count1(config%max_reps), count2(config%max_reps)
            integer :: r, n
            real(dp) :: elapsed(config%max_reps), gflops(config%max_reps), avg_elapsed, avg_gflops
            size_loop: do n=config%n_min,config%n_max,config%n_step
                if (allocated(a)) deallocate(a)
                if (allocated(b)) deallocate(b)
                allocate(a(n), b(n))
                call system_clock(count_rate=count_rate)
                test_loop: do r=1,config%max_reps
                    call random_number(a)
                    call random_number(b)
                    call system_clock(count=count1(r))
                    call test(a, b, c(r))
                    call system_clock(count=count2(r))
                end do test_loop
                elapsed = real(max(count2 - count1, 1_i64), dp)/real(count_rate, dp)
                gflops = 2.0_dp*real(n, dp)/1.0D+9/elapsed
                avg_elapsed = sum(elapsed)/real(config%max_reps, dp)
                avg_gflops = sum(gflops)/real(config%max_reps, dp)
                write(stdout,'(a48,a,i0,a,i0,a,f0.2,a,e13.6,a)') label,' completed ',config%max_reps,' reps of size ',n, &
                                                                 ' with average ',avg_gflops,' GFLOPS (',maxval(c),')'
                flush(stdout)
                write(config%output_fid,'(6a,i0,2(a,e13.6))') trim(adjustl(fc_ver())),config%delimiter, &
                                                              trim(adjustl(fc_opt())),config%delimiter, &
                                                              label,config%delimiter, &
                                                              n,config%delimiter, &
                                                              avg_elapsed,config%delimiter, &
                                                              avg_gflops
                flush(config%output_fid)
            end do size_loop
        end subroutine benchmark_one_dot_product_dp


        impure subroutine benchmark_one_matmul_sp(test, label, config)
            procedure(matmul_sp) :: test
            character(len=*), intent(in) :: label
            type(benchmark_config), intent(in) :: config
            real(sp), allocatable :: a(:,:), b(:,:), c(:,:,:)
            integer(i64) :: count_rate, count1(config%max_reps), count2(config%max_reps)
            integer :: r, n
            real(dp) :: elapsed(config%max_reps), gflops(config%max_reps), avg_elapsed, avg_gflops
            size_loop: do n=config%n_min,config%n_max,config%n_step
                if (allocated(a)) deallocate(a)
                if (allocated(b)) deallocate(b)
                if (allocated(c)) deallocate(c)
                allocate(a(n,n), b(n,n), c(n,n,config%max_reps))
                call system_clock(count_rate=count_rate)
                test_loop: do r=1,config%max_reps
                    call random_number(a)
                    call random_number(b)
                    call system_clock(count=count1(r))
                    call test(a, b, c(:,:,r))
                    call system_clock(count=count2(r))
                end do test_loop
                elapsed = real(max(count2 - count1, 1_i64), dp)/real(count_rate, dp)
                gflops = 2.0_dp*real(n, dp)*real(n, dp)*real(n - 1, dp)/1.0D+9/elapsed
                avg_elapsed = sum(elapsed)/real(config%max_reps, dp)
                avg_gflops = sum(gflops)/real(config%max_reps, dp)
                write(stdout,'(a48,a,i0,a,i0,a,f0.2,a,e13.6,a)') label,' completed ',config%max_reps,' reps of size ',n, &
                                                                 ' with average ',avg_gflops,' GFLOPS (',maxval(c),')'
                flush(stdout)
                write(config%output_fid,'(6a,i0,2(a,e13.6))') trim(adjustl(fc_ver())),config%delimiter, &
                                                              trim(adjustl(fc_opt())),config%delimiter, &
                                                              label,config%delimiter, &
                                                              n,config%delimiter, &
                                                              avg_elapsed,config%delimiter, &
                                                              avg_gflops
                flush(config%output_fid)
            end do size_loop
        end subroutine benchmark_one_matmul_sp


        impure subroutine benchmark_one_matmul_dp(test, label, config)
            procedure(matmul_dp) :: test
            character(len=*), intent(in) :: label
            type(benchmark_config), intent(in) :: config
            real(dp), allocatable :: a(:,:), b(:,:), c(:,:,:)
            integer(i64) :: count_rate, count1(config%max_reps), count2(config%max_reps)
            integer :: r, n
            real(dp) :: elapsed(config%max_reps), gflops(config%max_reps), avg_elapsed, avg_gflops
            size_loop: do n=config%n_min,config%n_max,config%n_step
                if (allocated(a)) deallocate(a)
                if (allocated(b)) deallocate(b)
                if (allocated(c)) deallocate(c)
                allocate(a(n,n), b(n,n), c(n,n,config%max_reps))
                call system_clock(count_rate=count_rate)
                test_loop: do r=1,config%max_reps
                    call random_number(a)
                    call random_number(b)
                    call system_clock(count=count1(r))
                    call test(a, b, c(:,:,r))
                    call system_clock(count=count2(r))
                end do test_loop
                elapsed = real(max(count2 - count1, 1_i64), dp)/real(count_rate, dp)
                gflops = 2.0_dp*real(n, dp)*real(n, dp)*real(n - 1, dp)/1.0D+9/elapsed
                avg_elapsed = sum(elapsed)/real(config%max_reps, dp)
                avg_gflops = sum(gflops)/real(config%max_reps, dp)
                write(stdout,'(a48,a,i0,a,i0,a,f0.2,a,e13.6,a)') label,' completed ',config%max_reps,' reps of size ',n, &
                                                                 ' with average ',avg_gflops,' GFLOPS (',maxval(c),')'
                flush(stdout)
                write(config%output_fid,'(6a,i0,2(a,e13.6))') trim(adjustl(fc_ver())),config%delimiter, &
                                                              trim(adjustl(fc_opt())),config%delimiter, &
                                                              label,config%delimiter, &
                                                              n,config%delimiter, &
                                                              avg_elapsed,config%delimiter, &
                                                              avg_gflops
                flush(config%output_fid)
            end do size_loop
        end subroutine benchmark_one_matmul_dp

end module benchmark_m
