      subroutine yswap (n,dx,incx,dy,incy)
*
* $Id: yswap.F 19697 2010-10-29 16:57:34Z d3y133 $
*
      implicit none
#include "y64.fh"
      double precision dx(*),dy(*),dtemp
      integer incx,incy,n
c
      INTGR4 incx4,incy4,n4
c
      if(n.le.0)return
c
      incx4=incx
      incy4=incy
      n4=n
c
      call dswap (n4,dx,incx4,dy,incy4)
      return
      end
