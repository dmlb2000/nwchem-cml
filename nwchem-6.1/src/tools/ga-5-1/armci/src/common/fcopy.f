      subroutine dcopy21(rows, cols, A, ald, buf, cur)
      integer*4 rows, cols
      integer*4 c, r, ald, cur 
      double precision A(ald,*), buf(ald) 
      cur = 0
      do c = 1, cols
         do r = 1, rows
            cur = cur+1
            buf(cur) = A(r,c)
         end do
      end do
      end

      subroutine dcopy31(rows, cols, planes, A, aldr, aldc, buf, cur)
      integer*4 rows, cols, planes
      integer*4 c, r, p, aldr, aldc, cur
      double precision A(aldr, aldc, *), buf(aldr)
      cur = 0
      do p = 1, planes 
         do c = 1, cols
            do r = 1, rows
               cur = cur+1
               buf(cur) = A(r,c,p)
            end do
         end do
      end do
      end

      subroutine dcopy12(rows, cols, A, ald, buf, cur)
      integer*4 rows, cols
      integer*4 c, r, ald, cur
      double precision A(ald,*), buf(ald)
      cur = 0
      do c = 1, cols
         do r = 1, rows
            cur = cur+1
            A(r,c) = buf(cur)
         end do
      end do
      end

      subroutine dcopy13(rows, cols, planes, A, aldr, aldc, buf, cur)
      integer*4 rows, cols, planes
      integer*4 c, r, p, aldr, aldc, cur
      double precision A(aldr, aldc, *), buf(aldr)
      cur = 0
      do p = 1, planes
         do c = 1, cols
            do r = 1, rows
               cur = cur+1
               A(r,c,p) = buf(cur)
            end do
         end do
      end do
      end

      subroutine dcopy2d_n(rows, cols, A, ald, B, bld)
      integer*4 rows, cols
      integer*4 c, r, ald, bld
      double precision A(ald,*), B(bld,*)
      do c = 1, cols
         do r = 1, rows
            B(r,c) = A(r,c)
         end do
      end do
      end

      subroutine dcopy2d_u(rows, cols, A, ald, B, bld)
      integer*4 rows, cols
      integer*4 c, r, ald, bld
      double precision A(ald,*), B(bld,*)
      integer*4 r1, ZERO, THREE
      double precision d1, d2, d3, d4
      parameter (ZERO=0)
      parameter (THREE=3)
      do c = 1, cols
      r1 = iand(max0(rows,ZERO),THREE)
      do r = 1, r1
c$$$         b(r,c) = a(r,c) + b(r,c) * 0
         b(r,c) = a(r,c)
      end do
      do r = r1 + 1, rows, 4
         d1 = a(r,c)
         d2 = a(r+1,c)
         d3 = a(r+2,c)
         d4 = a(r+3,c)
         b(r,c) = d1
         b(r+1,c) = d2
         b(r+2,c) = d3
         b(r+3,c) = d4
c$$$         b(r,c) = a(r,c) + b(r,c) * 0
c$$$         b(r+1,c) = a(r+1,c) + b(r+1,c) * 0
c$$$         b(r+2,c) = a(r+2,c) + b(r+2,c) * 0
c$$$         b(r+3,c) = a(r+3,c) + b(r+3,c) * 0
      enddo
      enddo
      end

      subroutine dcopy1d_n(A, B, n)
      integer*4 n,i 
      double precision A(n), B(n)
ccdir$ no_cache_alloc a,b
      do i = 1, n 
            B(i) = A(i)
      end do
      end

      subroutine dcopy1d_u(A, B, n)
      integer*4 n,n1,i,ZERO,THREE
      double precision A(n), B(n)
      double precision d1, d2, d3, d4
      parameter (ZERO=0)
      parameter (THREE=3)
      n1 = iand(max0(n,ZERO),THREE)
      do i = 1, n1
            B(i) = A(i)
      end do
      do i = n1+1, n, 4
         d1 = a(i)
         d2 = a(i+1)
         d3 = a(i+2)
         d4 = a(i+3)
         b(i) = d1
         b(i+1) = d2
         b(i+2) = d3
         b(i+3) = d4
      end do
      end

