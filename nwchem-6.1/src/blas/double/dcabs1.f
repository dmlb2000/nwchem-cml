      double precision function dcabs1(z)
*
* $Id: dcabs1.f 19695 2010-10-29 16:51:02Z d3y133 $
*
      double complex z,zz
      double precision t(2)
      equivalence (zz,t(1))
      zz = z
      dcabs1 = dabs(t(1)) + dabs(t(2))
      return
      end
