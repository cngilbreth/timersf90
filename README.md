Timers for Fortran
==================

timers.f90 is a Fortran module for timing Fortran codes, particularly scientific codes
which need to be optimized to use CPU time efficiently. Features include:

- Allows you to register and start/stop named timers, taking care of
  accumulating times when they are restarted.

- Handles nested calls of timers: the library transparently records all contexts
  in which a timer is called (i.e., all situations in which it is called when
  other timers are running). It can then provide detailed printout of the call
  hierarchy with times for each context and percentages of the total time.

- Currently supports up to 63 simultaneous timers.

I have used this library in several projects and find it to be very useful, so I
am making it available on github.

