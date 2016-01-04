      subroutine get_col(g_a,n,i,vec)
C$Id: get_col.F 19696 2010-10-29 16:53:42Z d3y133 $
      implicit none
      integer g_a,n,i
      double precision vec(n)
      call ga_get(g_a,
     *     1,n,i,i,vec,1)
      return
      end
      subroutine put_col(g_a,n,i,vec)
      implicit none
      integer g_a,n,i
      double precision vec(n)
      call ga_put(g_a,
     *     1,n,i,i,vec,1)
      return
      end
