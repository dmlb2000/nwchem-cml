      program test
      use FoX_wxml
      use FoX_wcml
      use FoX_common
      use m_wcml_stml, only:stmAddValue, stmAddArray
      implicit none
      type(xmlf_t), save::xf
      character*2 elsym(10),ta(10),na(10)
      double precision co(3,10), ch(10)
      
      call cmlBeginFile(xf, filename='testfile', unit=10)
      call cmlAddNamespace(xf, prefix='compchem',
     &               URI='http://www.xml-cml.org/dictionary/compchem/')
      call cmlAddNamespace(xf, prefix='nwchem',
     &               URI='http://www.nwchem-sw.org/dictionary/nwchem/')
      call cmlStartCml(xf, convention='convention:compchem',
     &                 validate=.true.)
      call cmlAddMolecule(xf,natoms=10,elements=elsym,
     &                    coords=co,
     &                    occupancies=ch)
c    &                    atomsIds=ta)
      call cmlEndCml(xf)
      call cmlFinishFile(xf)
      end


c     gfortran -c -fdefault-integer-8 -I/home/bert/FoX-Dev/objs/finclude test.F
