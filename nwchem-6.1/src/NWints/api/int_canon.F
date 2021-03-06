c $Id: int_canon.F 19696 2010-10-29 16:53:42Z d3y133 $
*
c:tex-% part of the internal API routines
c:tex-\subsection{int\_canon}
c:tex-This routine canonicalizes integral lables such that:
c:tex-$i \geq j$, $k \geq l$, and $ij \geq kl$
c:tex-
c:tex-{\it Syntax:}
c:tex-\begin{verbatim}
      subroutine int_canon(i, j, k, l, ii, jj, kk, ll)
c:tex-\end{verbatim}
      implicit none
c:tex-\begin{verbatim}
      integer i,j,k,l      ! [input] labels
      integer ii,jj,kk,ll  ! [output] canonical labels
c:tex-\end{verbatim}
      integer iii,jjj
#if defined(DEBUG)
#include "stdio.fh"
      logical warn
#endif
c
c     return in ii,jj,kk,ll the cannonically ordered i,j,k,l
c
c     (i>=j) >= (k>=l)
c
#if defined(DEBUG)
      warn = .false.
#endif
      if (i .ge. j) then
        ii = i
        jj = j
      else
#if defined(DEBUG)
        warn = .true.
#endif
        ii = j
        jj = i
      endif
      if (k .ge. l) then
        kk = k
        ll = l
      else
#if defined(DEBUG)
        warn = .true.
#endif
        kk = l
        ll = k
      endif
      if (ii.lt.kk .or. (ii.eq.kk .and. jj.lt.ll)) then
#if defined(DEBUG)
        warn = .true.
#endif
        iii = ii
        jjj = jj
        ii = kk
        jj = ll
        kk = iii
        ll = jjj
      endif
#if defined(DEBUG)
      if (warn) then
        write(luout,10000)i,j,k,l
        write(luout,10001)ii,jj,kk,ll
      endif
10000 format(1x,'old:',4(i5))
10001 format(1x,'new:',4(i5))
#endif
c
      end
