      logical function xc_chkdispauto()
C
      implicit none
c
c     see if any functional has automatic dispersion included
c
#include "cdft.fh"
c     note in cdft.fh, they define cfac(numfunc) and xfac(numfunc)
c 
      logical lhasautodisp
      lhasautodisp = .false.
c
c     beck97-d, ssb-d
      lhasautodisp = xccomb(37).or.xccomb(46)   
c
      if (lhasautodisp) then
         xc_chkdispauto = .true.
      else
         xc_chkdispauto = .false.
      endif
c
      return
      end
c $Id: xc_chkdispauto.F 21176 2011-10-10 06:35:49Z d3y133 $
