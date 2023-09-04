program main
use, non_intrinsic :: constants_m, only: stdout, i64
use, non_intrinsic :: benchmark_m, only: benchmark_config, echo_config, benchmark_all
implicit none

    type(benchmark_config) :: config
    integer :: argc, i
    character(len=128) :: keyword_buffer, argv_buffer
    character(len=:), allocatable :: output_file
    integer(i64) :: date_time_values(8)

    call date_and_time(values=date_time_values)
    write(argv_buffer,'(i0,a,5(i2.2,a),i3.3,a)') date_time_values(1),'-', &        ! year
                                                 date_time_values(2),'-', &        ! month
                                                 date_time_values(3),'_', &        ! day
                                                 date_time_values(5),'-', &        ! hour
                                                 date_time_values(6),'-', &        ! minute
                                                 date_time_values(7),'-', &        ! second
                                                 date_time_values(8),'_output.csv' ! millisecond
    output_file = trim(adjustl(argv_buffer))

    argc = command_argument_count()
    if (argc > 1) then
        if (mod(argc, 2) /= 0) then
            write(stdout,'(a)') 'invalid input arguments received, valid keywords are:'
            write(stdout,'(a)') '    --max_reps    - number of repetitions to average for benchmark results'
            write(stdout,'(a)') '    --output_file - output file name'
            write(stdout,'(a)') '    --delimiter   - output file delimiter'
            write(stdout,'(a)') '    --n_min       - minimum dot product vector length/matrix multiplication side length'
            write(stdout,'(a)') '    --n_max       - maximum dot product vector length/matrix multiplication side length'
            write(stdout,'(a)') '    --n_step      - step size from n_min to n_max'
            write(stdout,'(a,i0,a)') 'received ',argc,' arguments'
            do i=1,argc
                call get_command_argument(i, keyword_buffer)
                write(stdout,'(a,i0,a)') 'argument ',i,': '//keyword_buffer
            end do
            error stop 'try again'
        end if
        do i=1,argc,2
            call get_command_argument(i, keyword_buffer)
            call get_command_argument(i+1, argv_buffer)
            select case (trim(adjustl(keyword_buffer)))
                case ('--max_reps')
                    read(argv_buffer,*) config%max_reps
                case ('--output_file')
                    output_file = trim(adjustl(argv_buffer))
                case ('--delimiter')
                    read(argv_buffer,*) config%delimiter
                case ('--n_min')
                    read(argv_buffer,*) config%n_min
                case ('--n_max')
                    read(argv_buffer,*) config%n_max
                case ('--n_step')
                    read(argv_buffer,*) config%n_step
                case default
                    write(stdout,'(a)') 'unknown keyword argument, read: "'//keyword_buffer//'"'
                    error stop 'unknown keyword argument: '//trim(adjustl(keyword_buffer))
            end select
        end do
    end if

    call echo_config(config)

    open(newunit=config%output_fid, file=output_file, status='unknown', action='write', position='append')
    call benchmark_all(config)
    close(config%output_fid)

    write(stdout,'(a)') 'COMPLETE!!'

end program main
