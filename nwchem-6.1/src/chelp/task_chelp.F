      function task_chelp(rtdb)
      implicit none
#include "errquit.fh"
      integer rtdb
      logical task_chelp
c
      write(*,*) "Hello from task chelp"
      call chelp_grid_gen(rtdb)
      task_chelp = .true.
c
c     create grids
c     -----------
    
      return
      end
c $Id: task_chelp.F 21176 2011-10-10 06:35:49Z d3y133 $
