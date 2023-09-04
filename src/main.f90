program main
use, non_intrinsic :: constants_m, only: stdout, i64
use, non_intrinsic :: benchmark_m, only: benchmark_all
implicit none

    integer :: max_reps, fid, argc, i
    character(len=128) :: keyword_buffer, argv_buffer
    character(len=:), allocatable :: output_file
    integer(i64) :: date_time_values(8)

    max_reps = 100
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
        if (mod(argc, 2) /= 0) error stop 'please provide matching keyword arguments, valid options: --max_reps --output_file'
        do i=1,argc,2
            call get_command_argument(i, keyword_buffer)
            call get_command_argument(i+1, argv_buffer)
            select case (trim(adjustl(keyword_buffer)))
                case ('--max_reps')
                    read(argv_buffer,*) max_reps
                case ('--output_file')
                    output_file = trim(adjustl(argv_buffer))
                case default
                    write(stdout,'(a)') 'unknown keyword argument, read: "'//keyword_buffer//'"'
                    error stop 'unknown keyword argument: '//trim(adjustl(keyword_buffer))
            end select
        end do
    end if

    write(stdout,'(a,i0)') 'output_file: '//output_file//', max_reps: ',max_reps
    
    open(newunit=fid, file=output_file, status='unknown', action='write', position='append')
    call benchmark_all(max_reps, fid)
    close(fid)

end program main
