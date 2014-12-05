! Simple test program for the timers module.
! Note, the first time I run the program after compiling is when the times seem
! to be the most consistent. Subsequent runs the times change quite a bit. The
! CPU is doing something funny here.

program test
  use mod_timers
  implicit none

  call test_timers()

contains

  subroutine  loop()
    implicit none
    integer :: i,j,k

    do k = 1,1000
       do i=1,1000000
          j = j + i
       end do
    end do
  end subroutine loop

  subroutine test_timers()
    ! Not an automated test. Just some scratch code to try it out.
    implicit none

    integer :: outer1,outer2,inner1,inner2, j=0,i,k
    real(8) :: t3

    call add_timer("outer1", outer1)
    call add_timer("outer2", outer2)
    call add_timer("inner1", inner1)
    call add_timer("inner2", inner2)

    ! Result should be:
    !  outer1 has time 2*T
    !  outer2 has time T
    !  inner1 has time 2*T
    !  inner2 has time T
    ! "time in outer1:" is T

    call start_timer(outer1)
      call start_timer(inner1)
        do k = 1,1000
           do i=1,1000000
              j = j + i
           end do
        end do
        t3 = get_time(outer1)
        write (*,*) "time in outer1: ", t3
      call stop_timer(inner1)

      call start_timer(inner1)
        call start_timer(inner2)
          do k = 1,1000
             do i=1,1000000
                j = j + i
             end do
          end do
        call stop_timer(inner2)
      call stop_timer(inner1)

    call stop_timer(outer1)

    call start_timer(outer2)
      do k = 1,1000
         do i=1,1000000
            j = j + i
         end do
      end do
    call stop_timer(outer2)

    call print_all_timers_flat(6)
    write (6,*) ""
    call print_all_timers(6)
    call clear_timers()
  end subroutine test_timers
end program test
