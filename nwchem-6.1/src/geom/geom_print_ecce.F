      subroutine geom_print_rtdb_ecce(rtdb)
*
* $Id: geom_print_ecce.F 19708 2010-10-29 18:04:21Z d3y133 $
*
#include "geom.fh"
#include "errquit.fh"
      integer rtdb, geom
c
      if (.not. geom_create(geom, 'geometry'))  call errquit
     $     ('geom_print_rtdb_ecce: geom_create failed',0, GEOM_ERR)
      if (.not. geom_rtdb_load(rtdb, geom, 'geometry')) call errquit
     $     ('geom_print_rtdb_ecce: geom_rtdb_load failed',0, RTDB_ERR)
      call geom_print_ecce(geom)
      if (.not. geom_destroy(geom)) call errquit
     $     ('geom_print_rtdb_ecce: geom destroy failed',0, GEOM_ERR)
c
      end
      subroutine geom_print_ecce(geom)
      use ecce_print_m
      implicit none
#include "nwc_const.fh"
#include "geomP.fh"
#include "mafdecls.fh"
      integer geom
      logical geom_tag_to_element
      external geom_tag_to_element
c
c     Output the tags, charges, and cartesian coordinates (Angs) 
c     to the ecce output file
c
      logical geom_check_handle
      external geom_check_handle
c
      integer nat,i,atn
      character*2 elsym(nw_max_atom)
      character*16 elem
      double precision mycoords(3,nw_max_atom)
c
      if (.not. geom_check_handle(geom,'geom_print_ecce')) return
c
      if (is_ecce_print_on()) then
      nat = ncenter(geom)
      do i=1,nat
       if (.not.geom_tag_to_element(tags(i,geom),elsym(i),elem,atn))
     $    call errquit('geom_print_ecce: tag not atom', 0, 911)
      enddo
      call dscal(nat*3, 1.0d0/angstrom_to_au, coords(1,1,geom), 1)
      call ecce_cml_print_geom(nat,elsym,coords(1,1,geom),
     &                         tags(1,geom),names(geom))
*     **** output amatrix in angstroms ****
      if (isystype(geom).eq.3) then
         call dscal(3*3, 1.0d0/angstrom_to_au, amatrix(1,1,geom), 1)
c        call ecce_print2('lattice vectors', mt_dbl,
c    $     amatrix(1,1,geom), 3, 3, 3)
         call ecce_do_print('lattice vectors', amatrix(1,1,geom),3,3,3)
         call dscal(3*3, angstrom_to_au, amatrix(1,1,geom), 1)
      end if
      end if 
c
      end

      subroutine ecce_cml_print_geom(nat,elsym,coord,tags,name)
      use ecce_print_m
      integer nat
      character*2 elsym(nat)
      character*16 tags(nat)
      character*256 name
      double precision coord(3,nat),charge(nat)
      call cmlAddMolecule(xf,natoms=nat,elements=elsym,coords=coord,
     &                    atomIds=tags,style='cartesian',
     &                    id=trim(name))
      return
      end

      subroutine ecce_do_print(string,matrix,ld,l1,l2)
      use ecce_print_m
      implicit none
#include "mafdecls.fh"
      character*(*) string
      double precision matrix(ld,l1)
      integer ld, l1, l2
      call ecce_print2(string,mt_dbl,matrix,ld,l1,l2)
      return
      end
