*
* $Id: nwpw_filefind.F 19707 2010-10-29 17:59:36Z d3y133 $
*

*     ***********************************
*     *									*
*     *	 	  nwpw_filefind 				*
*     *									*
*     ***********************************

      logical function nwpw_filefind(fname)
      implicit none
      character*50 fname

#include "tcgmsg.fh"
#include "msgtypesf.h"

*     *** local variables ***
      integer MASTER,taskid
      parameter(MASTER=0)

      logical found
      integer idum,msglen
      character*255 full_filename

      call Parallel_taskid(taskid)
      if (taskid.eq.MASTER) then
         call util_file_name_noprefix(fname,.false.,
     >                             .false.,
     >                       full_filename)
         inquire(file=full_filename,exist=found)

         idum = 0
         if (found) idum = 1
      end if
      msglen = 1
      call BRDCST(9+MSGINT,idum,mitob(msglen),MASTER)

      if (idum.eq.1) then
         found = .true.
      else
         found = .false.
      end if

      nwpw_filefind = found
      return
      end

