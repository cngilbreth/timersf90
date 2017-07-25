! Simple test program for the timers module.
! Note, the first time I run the program after compiling is when the times seem
! to be the most consistent. Subsequent runs the times change quite a bit. The
! CPU is doing something funny here.

program test
  use mod_timers
  implicit none

  integer :: t_outer,t_inner,t_something

  call test_timers()

contains

  subroutine  something()
    implicit none
    integer :: i,j
    integer, parameter :: n = 1024
    real*8, allocatable :: A(:,:), C(:,:)

    allocate(A(n,n),C(n,n))

    C = 0.d0
    do j=1,n
       do i=1,n
          A(i,j) = 1.d0/(i + j)
       end do
       C(j,j) = 1.d0
    end do

    do i=1,2
       write (*,'(a,i0,a)') "inner iteration ", i, " ..."
       call start_timer(t_inner)
       C = matmul(C,A)
       call stop_timer(t_inner)
    end do
    write (123,*) C(1,1)
  end subroutine something


  subroutine test_timers()
    ! Not an automated test. Just some scratch code to try it out.
    ! It does include some manual timers using system_clock for comparison.
    implicit none

    integer(8) :: k

    call add_timer("outer", t_outer)
    call add_timer("something", t_something)
    call add_timer("inner_iteration", t_inner)

    call start_timer(t_outer)
    do k=1,10
       call start_timer(t_something)
       call something()
       call stop_timer(t_something)
    end do
    call stop_timer(t_outer)


    call print_all_timers_flat(6)
    write (6,*) ""
    call print_all_timers(6)
    call clear_timers()
  end subroutine test_timers

end program test
