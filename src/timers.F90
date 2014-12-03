! timers.F90: Timers module for Fortran 90
! - Allows you to define and start/stop timers.
! - Handles nested calls of timers and provides a detailed printout with
!   percentages of the "total" time (defined as the time spent in all outermost
!   contexts).
! - Supports up to 63 timers
! - To use: Call add_timer(), start_timer(), stop_timer(),
!   print_all_timers_flat(), and print_all_timers(). See example at the bottom.
! C.N.G 2010, 2014
module mod_timers
  implicit none
  save

  ! There are two ways to record the time:
  !  1. cpu_time measures CPU time (excludes time spent in other programs)
  !  2. system_clock measures walltime
  ! cpu_time is probably preferred.
  integer, parameter :: CPU_TIME_ = 1, SYSTEM_CLOCK_ = 2
  integer, parameter :: TIME_TYPE = CPU_TIME_
  integer(8) :: count_rate  ! Conversion for system_clock()


  ! Timing info for a timer in a particular context.
  ! When a timer is started, there are usually other timers going already.
  ! The set of which timers are already going is called a "context", and
  ! is represented by a set of binary on/off flags (the key).
  ! We record times in each context separately.
  type context
     ! Binary description of the context
     integer(8) :: key
     ! # of times this timer has been started in this context
     integer(8) :: ncalls
     ! for cpu_time
     real(8)    :: tstart, tstop
     ! for system_clock
     integer(8) :: itstart, itstop
     ! Total time so far in this context
     real(8) :: tsum
  end type context


  ! Structure to identify a timer and the contexts in which it's been called
  type timer
     integer :: id
     character(len=64) :: name
     ! Array of context-specific timer structures
     type(context), allocatable :: contexts(:)
     ! When this timer is running, this is the index of the relevant context in
     ! contexts(:). When not running, this is the index of the last context in
     ! which it was executed.
     integer :: last_context_index
  end type timer

  integer :: ntimers = 0
  type(timer), allocatable, target :: timers(:)
  integer(8) :: current_context = 0

contains

  elemental subroutine init_context(c)
    implicit none
    type(context), intent(inout) :: c
    c%key = -1
    c%ncalls  = 0
    c%tstart  = 0
    c%tstop   = 0
    c%itstart = 0
    c%itstop  = 0
    c%tsum    = 0
  end subroutine init_context


  subroutine add_timer(name,id)
    implicit none
    character(len=*), intent(in) :: name
    integer, intent(out) :: id
    type(timer), pointer :: t

    integer(8) :: count

    call allocate_timers_mabye(1)
    if (count_rate .eq. 0) then
       call system_clock(count,count_rate)
    end if

    ntimers = ntimers + 1
    id = ntimers
    if (id > bit_size(current_context)-1) then
       ! Can't store a context for > 63 timers in an 8-byte integer.
       stop "Error: too many timers!"
    end if

    t => timers(id)
    t%id = id
    t%name = name
    allocate(t%contexts(4))
    call init_context(t%contexts)
    t%last_context_index = 1
  end subroutine add_timer


  subroutine clear_timers()
    implicit none
    ntimers = 0
  end subroutine clear_timers


  subroutine allocate_timers_mabye(n)
    implicit none
    integer, intent(in) :: n
    integer :: new_size
    type(timer), allocatable :: timers_tmp(:)

    if (.not. allocated(timers)) then
       allocate(timers(n+15))
       return
    end if

    if (ntimers + n > size(timers)) then
       new_size = size(timers)
       do
          new_size = new_size*2
          if (new_size >= size(timers) + n) exit
       end do
       allocate(timers_tmp(size(timers)))
       timers_tmp = timers
       deallocate(timers)
       allocate(timers(new_size))
       timers(1:ntimers) = timers_tmp
       deallocate(timers_tmp)
    end if
  end subroutine allocate_timers_mabye


  subroutine start_timer(id)
    implicit none
    integer, intent(in) :: id

    type(timer),   pointer :: t
    type(context), pointer :: contexts(:), c
    integer :: i, nc

    if (id < 1 .or. id > ntimers) stop "Error: invalid timer id"
    t => timers(id)
    if (btest(current_context,id-1)) then
       write (0,*) "Error: timer ", trim(adjustl(t%name)), " already running"
       stop
    end if

    ! Find the context we're running in, or add a new one if necessary
    ! TODO: Optimize by first trying the last known context?
    contexts => t%contexts
    nc = size(contexts)
    do i=1,nc
       c => contexts(i)
       if (c%key < 0 .or. c%key == current_context) exit
    end do
    if (i > nc) then
       ! Separate function to reduce code size
       call allocate_more_contexts(t)
       c => t%contexts(i)
    end if

    if (c%key < 0) then
       c%key = current_context
    end if
    current_context = ibset(current_context, id-1)

    if (TIME_TYPE == CPU_TIME_) then
       call cpu_time(c%tstart)
    else if (TIME_TYPE == SYSTEM_CLOCK_) then
       call system_clock(c%itstart)
    else
       stop "Error in timers.f90: invalid time_type"
    end if

    t%last_context_index = i
    c%ncalls = c%ncalls + 1
  end subroutine start_timer


  subroutine allocate_more_contexts(t)
    implicit none
    type(timer), intent(inout) :: t

    type(context), allocatable :: tmp(:)
    integer :: nc
    nc = size(t%contexts)
    allocate(tmp(nc))
    tmp = t%contexts
    deallocate(t%contexts)
    allocate(t%contexts(nc*2))
    t%contexts(1:nc) = tmp(1:nc)
    call init_context(t%contexts(nc+1:))
  end subroutine allocate_more_contexts


  subroutine stop_timer(id)
    implicit none
    integer, intent(in) :: id

    type(timer), pointer :: t
    type(context), pointer :: contexts(:), c


    if (id < 1 .or. id > ntimers) stop "Error: invalid timer id"
    t => timers(id)
    if (.not. btest(current_context,id-1)) stop "Error: timer already stopped"
    contexts => t%contexts

    current_context = ibclr(current_context, id-1)
    c => contexts(t%last_context_index)
    if (c%key .ne. current_context) then
       write (0,*) "Error: tried to stop timer ", trim(t%name), " in wrong context!"
       write (0,*) "Last context index: ", t%last_context_index
       write (0,*) "Current context: ", current_context
       write (0,*) "Old context: ", c%key
       stop
    end if

    if (TIME_TYPE == CPU_TIME_) then
       call cpu_time(c%tstop)
       c%tsum = c%tsum + c%tstop-c%tstart
    else if (TIME_TYPE == SYSTEM_CLOCK_) then
       call system_clock(c%itstop)
       c%tsum = c%tsum + (c%itstop-c%itstart)/real(count_rate,kind(1.d0))
    else
       stop "Error in timers.f90: invalid time_type"
    end if

  end subroutine stop_timer


  real(8) function get_time(id) result(time)
    ! Get time elapsed for the timer indicated by 'id', allowing for the
    ! possibility that the timer is currently running.
    implicit none
    integer, intent(in) :: id

    type(timer),   pointer :: t
    type(context), pointer :: c
    real(8)    :: tstop
    integer :: itstop, i
    logical :: running
    integer(8) :: parent_context

    if (id < 1 .or. id > ntimers) stop "Error: invalid timer id"
    t => timers(id)

    running = btest(current_context,id-1)
    parent_context = ibclr(current_context,id-1)

    time = 0.d0
    do i=1,size(t%contexts)
       c => t%contexts(i)
       if (c%key >= 0) then
          time = time + c%tsum
          if (running .and. c%key == parent_context) then
             if (TIME_TYPE == CPU_TIME_) then
                call cpu_time(tstop)
                time = time + tstop-c%tstart
             else if (TIME_TYPE == SYSTEM_CLOCK_) then
                call system_clock(itstop)
                time = time + (itstop-c%itstart)/real(count_rate,kind(1.d0))
             else
                stop "Error in timers.f90: invalid time_type"
             end if
          end if
       end if
    end do
  end function get_time


  subroutine calc_total_time(total)
    ! Sum time in all outermost contexts. We'll call this the total.
    implicit none
    real(8), intent(out) :: total

    integer :: id, ic
    type(timer), pointer   :: t
    type(context), pointer :: c

    total = 0.d0
    do id=1,ntimers
       t => timers(id)
       do ic=1,size(t%contexts)
          c => t%contexts(ic)
          if (c%key == 0) total = total + c%tsum
       end do
    end do
  end subroutine calc_total_time


  subroutine print_all_timers_flat(unit)
    implicit none
    integer, intent(in) :: unit
    integer :: id, ic
    type(timer), pointer   :: t
    type(context), pointer :: c

    real(8) :: time, total
    integer(8) :: ncalls

    if (current_context .ne. 0) then
       write (*,*) "WARNING: There are timers still running. They should be &
            &stopped"
       write (*,*) "before printing out the timers. Some numbers may be&
            & incorrect."
    end if

    call calc_total_time(total)

    write (unit,'(a2,tr1,a46,tr2,a10,tr2,a12,tr2,a7)') &
         "id", "Timer name                                    ", &
         "# of calls", "time (s)", "% total"
    write (unit,'(2("*"),tr1,46("*"),tr2,10("*"),tr2,12("*"),tr2,7("*"))')
    do id=1,ntimers
       t => timers(id)
       time = 0.d0
       ncalls = 0
       do ic=1,size(t%contexts)
          c => t%contexts(ic)
          if (c%key >= 0) then
             time = time + c%tsum
             ncalls = ncalls + c%ncalls
          end if
       end do
       write (unit,'(i2,tr1,a46,tr2,i10,tr2,f12.4,tr2,f6.2,"%")') &
            t%id, t%name, ncalls, time, time/total * 100.d0
    end do
  end subroutine print_all_timers_flat


  recursive subroutine print_all_timers_aux(unit,icontext,depth,nsub,tsub,&
       total,prnt)
    ! Recursively print all timers in a given context, and their subtimers.
    implicit none
    integer,    intent(in)  :: unit,depth
    integer(8), intent(in)  :: icontext
    integer,    intent(out) :: nsub
    real(8),    intent(out) :: tsub
    real(8),    intent(in)  :: total
    logical,    intent(in)  :: prnt

    integer(8) :: isubcontext
    integer :: id, ic, nsub1
    real(8) :: tsub1, tinternal
    type(timer),   pointer   :: t
    type(context), pointer :: c

    character(len=60) :: spaces, str

    nsub = 0; tsub = 0
    spaces = ' '
    do id=1,ntimers
       t => timers(id)
       do ic=1,size(t%contexts)
          c => t%contexts(ic)
          if (c%key == icontext) then
             nsub = nsub + 1
             tsub = tsub + c%tsum
             isubcontext = ibset(icontext,id-1)
             if (prnt) then
                write (str,'(a,a)') spaces(1:depth*2), trim(t%name)
                write (unit,'(i2,tr1,a46,tr2,i10,tr2,f12.4,tr2,f6.2,"%")') &
                     id, str, c%ncalls, c%tsum, c%tsum/total*100.d0
             end if
             ! Add up the amount of time spent in subtimers
             call print_all_timers_aux(unit,isubcontext,depth+1,nsub1,tsub1,&
                  total,.false.)
             if (nsub1 > 0 .and. prnt) then
                ! Print amount of time spent in this timer, and not in subtimers
                tinternal = c%tsum - tsub1
                write (str,'(a,a)') spaces(1:(depth+1)*2), '(internal)'
                write (unit,'(a2,tr1,a46,tr2,a10,tr2,f12.4,tr2,f6.2,"%")') &
                     '', str, '-', tinternal, tinternal/total*100.d0
             end if
             ! Print all the subtimers
             call print_all_timers_aux(unit,isubcontext,depth+1,nsub1,tsub1,&
                  total,prnt)
          end if
       end do
    end do
  end subroutine print_all_timers_aux


  subroutine print_all_timers(unit)
    implicit none
    integer, intent(in) :: unit

    integer :: nsub
    real(8) :: tsub, total

    call calc_total_time(total)

    write (unit,'(a2,tr1,a46,tr2,a10,tr2,a12,tr2,a7)') &
         "id", "Timer name                                    ", &
         "# of calls", "time (s)", "% total"
    write (unit,'(2("*"),tr1,46("*"),tr2,10("*"),tr2,12("*"),tr2,7("*"))')

    call print_all_timers_aux(unit,0_8,0,nsub,tsub,total,.true.)
  end subroutine print_all_timers


  subroutine test_timers()
    ! Not an automated test. Just some scratch code to try it out.
    implicit none

    integer :: id1,id2,id3,id4, j=0,i,k
    real(8) :: t3

    call add_timer("outer timer",id1)
    call add_timer("inner timer",id2)
    call add_timer("inner timer 2",id3)
    call add_timer("outer timer 2",id4)

    call start_timer(id1)

    do k = 1,1000
       do i=1,1000000
          j = j + i
       end do
    end do

    call start_timer(id2)

    do k = 1,10000
       call start_timer(id3)
       call stop_timer(id3)
    end do

    call stop_timer(id2)

    call start_timer(id3)
    do k = 1,1000
       do i=1,1000000
          j = j + i
       end do
    end do
    call stop_timer(id3)

    call start_timer(id3)
    do k = 1,1000
       do i=1,1000000
          j = j + i
       end do
    end do
    t3 = get_time(id3)
    write (*,*) "Time in id3: ", t3
    call stop_timer(id3)

    call stop_timer(id1)

    call start_timer(id4)
    do k = 1,1000
       do i=1,500000
          j = j + i
       end do
    end do
    call stop_timer(id4)

    call print_all_timers_flat(6)
    write (6,*) ""
    call print_all_timers(6)
    call clear_timers()
  end subroutine test_timers


end module mod_timers


#ifdef TEST_TIMERS
program test
  use mod_timers
  call test_timers()
end program test
#endif
