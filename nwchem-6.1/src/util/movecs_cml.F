      subroutine movecs_cml(nbf, nmo, eval, occ, sym,
     $     g_movecs, geom, basis, nsets, myid)
*
* $Id: movecs_ecce.F 19707 2010-10-29 17:59:36Z d3y133 $
*
      use ecce_print_m
      implicit none
#include "errquit.fh"
#include "global.fh"
#include "mafdecls.fh"
#include "inp.fh"
      integer nbf               ! [input] no. of basis functions
      integer nmo               ! [input] no. of MOs
      double precision eval(*)  ! [input] MO energies
      double precision occ(*)   ! [input] MO occupation numbers
      character*255 myid        ! [input] movecs ID name
      integer sym(*)            ! [input] MO symmetries
      integer g_movecs(2)       ! [input] GA with vectors
      integer geom              ! [input] Geometry handle
      integer basis             ! [input] Basis handle
      integer nsets             ! [input] Number of orbital sets
                                !         1 = RHF, alpha=beta
                                !         2 = UHF, alpha and beta set
c
      integer l_v, k_v, l_ang, k_ang, l_sym, k_sym
      integer maxireps,i,ioff,j
      parameter (maxireps=20)
      integer nop, nir, class_dim(maxireps)
      character*8 zir(maxireps), zclass(maxireps)
      character*20 zname
      double precision chars(maxireps*maxireps)
      logical sym_char_table
      external sym_char_table
c
      if (ecce_print_enabled.eq.0) return
c
c     Get the MOs
c
      if (.not. ma_push_get(mt_dbl, nsets*nmo*nbf, 'mocml', l_v, k_v))
     $     call errquit('movecs_cml: insufficient memory',0, MA_ERR)
      do i=1,nsets
         ioff=(i-1)*nmo*nbf
         call ga_get(g_movecs(i),1,nbf,1,nmo,dbl_mb(k_v+ioff),nbf)
      enddo
c
c     Get basis function information
c
      if (.not. ma_push_get(mt_byte,nbf*16,'bfancml',l_ang,k_ang))
     $     call errquit('movecs_cml: insufficient memory',0, MA_ERR)
      call bas_vec_info(basis, byte_mb(k_ang))
c
c     Get symmetry information for orbitals
c
      if (.not. ma_push_get(mt_byte,nsets*nmo*8,'symcml',l_sym,k_sym))
     $     call errquit('movecs_cml: insufficient memory',0, MA_ERR)
      call sym_group_name(geom, zname)
      if (.not. sym_char_table(zname, nop, nir, class_dim,
     $           zir, zclass, chars)) call errquit
     $           ('movecs_anal: no char table available ',geom,
     &       UNKNOWN_ERR)
      call cml_sym_info(geom, nsets*nmo, byte_mb(k_sym), sym, zir)
c
c     Clear path stuff from myid
c
      do i=255,1,-1
         if (myid(i:i).eq."/") then
            do j=1,i
               myid(j:j)=" "
            enddo
         endif
      enddo
c
c     Do the writing
c
      call movecs_cml_print(nbf,nmo,byte_mb(k_ang),
     &                      eval,occ,byte_mb(k_sym),dbl_mb(k_v),
     &                      eval(nbf+1),occ(nbf+1),byte_mb(k_sym+8*nmo),
     &                      dbl_mb(k_v+nmo*nbf),nsets,myid)
c
c     Clean up memory
c
      if (.not. ma_pop_stack(l_sym)) call errquit
     $     ('movecs_ecce: ma stack is corrupt', 0, MA_ERR)
      if (.not. ma_pop_stack(l_ang)) call errquit
     $     ('movecs_ecce: ma stack is corrupt', 0, MA_ERR)
      if (.not. ma_pop_stack(l_v)) call errquit
     $     ('movecs_ecce: ma stack is corrupt', 0, MA_ERR)
c
      return
      end
c
      subroutine movecs_cml_print(nbf,nmo,cang,evala,occa,csyma,moa,
     &                            evalb,occb,csymb,mob,nsets,myid)
      use ecce_print_m
      implicit none
      integer nbf, nmo, nsets
      double precision evala(nmo), occa(nmo), moa(nbf,nmo)
      double precision evalb(nmo), occb(nmo), mob(nbf,nmo)
      character*8 csyma(nmo), csymb(nmo)
      character*16 cang(nbf)
      character*255 myid
c
      if (nsets.eq.1) then
         call cmlAddMolecularOrbitals(xf=xf,nbasisfunctions=nbf,
     &             norbitals=nmo,aotypes=cang,myid=adjustl(myid),
     &             alphaeigenvalues=evala,alphaoccupancies=occa,
     &             alphasymmetries=csyma,alphavectors=moa)
      else
         call cmlAddMolecularOrbitals(xf=xf,nbasisfunctions=nbf,
     &             norbitals=nmo,aotypes=cang,myid=adjustl(myid),
     &             alphaeigenvalues=evala,alphaoccupancies=occa,
     &             alphasymmetries=csyma,alphavectors=moa,
     &             betaeigenvalues=evalb,betaoccupancies=occb,
     &             betasymmetries=csymb,betavectors=mob)
      endif
c
      return
      end
c
      subroutine cml_sym_info(geom,ilen,symarray,sym,zir)
      implicit none
      integer geom,ilen, sym(ilen)
      character*8 symarray(ilen)
      character*8 zir(20)
      integer i
c
      do i=1, ilen
         symarray(i)=zir(sym(i))
      enddo
c
      return
      end
