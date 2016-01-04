*
* $Id: neb.F 19708 2010-10-29 18:04:21Z d3y133 $
*
      logical function neb(rtdb)
      implicit none
      integer rtdb
      logical nebnew

#include "global.fh"
#include "mafdecls.fh"
#include "rtdb.fh"
#include "neb_data.fh"

      neb=.true.
      call neb_init(rtdb)
      call runall_bead_list(neb_list)
      
      
      return
      end
c

