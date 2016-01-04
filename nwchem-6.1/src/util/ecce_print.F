      module ecce_print_m
!
      use FoX_wxml
      use FoX_wcml
      use FoX_common
      use m_wcml_stml, only:stmAddValue, stmAddArray
      type(xmlf_t), save::xf
      integer, save::ecce_print_enabled
      interface ecce_print2
          module procedure ecceP2D_Int
          module procedure ecceP2D_DP
          module procedure ecceP2D_Int3D
          module procedure ecceP2D_DP3D
          module procedure ecceP2D_IntRef
          module procedure ecceP2D_DPRef
          module procedure ecceP2D_Int3DRef
          module procedure ecceP2D_DP3DRef
      end interface

      interface ecce_print1
          module procedure ecceP1D_Int
          module procedure ecceP1D_DP
          module procedure ecceP1D_IntSc
          module procedure ecceP1D_DPSc
          module procedure ecceP1D_IntRef
          module procedure ecceP1D_DPRef
          module procedure ecceP1D_IntScRef
          module procedure ecceP1D_DPScRef
      end interface

      interface ecce_print1_char
          module procedure eccePChar_Array8
          module procedure eccePChar_Scalar2
          module procedure eccePChar_Array8Ref
          module procedure eccePChar_Scalar2Ref
      end interface

      contains

      subroutine ecceP2D_Int(key,mb_type,array,ld,length1,length2)
!
! Print 2D array of ints
!
      integer ld,length1,length2,mb_type,i
      integer array(ld,length2)
      character(LEN=*) key
      character(LEN=80) key2
      if (ecce_print_enabled.eq.0) return
      key2=''
      do i=1,len_trim(key)
         if (key(i:i).eq.' ') then
            key2(i:i)='_'
         else
            key2(i:i)=key(i:i)
         endif
      enddo
      call cmlAddProperty(xf, units='unit:none', nrows=length1, 
     &                    ncols=length2, value=array, 
     &                    dictRef='nwchem:'//trim(key2))
      end subroutine

      subroutine ecceP2D_IntRef(key,mb_type,array,ld,length1,length2,
     &                           iat,mytag)
!
! Print 2D array of ints
!
      integer ld,length1,length2,mb_type,i,iat
      integer array(ld,length2)
      character(LEN=*) key,mytag
      character(LEN=80) key2
      character(LEN=10) num
      if (ecce_print_enabled.eq.0) return
      key2=''
      do i=1,len_trim(key)
         if (key(i:i).eq.' ') then
            key2(i:i)='_'
         else
            key2(i:i)=key(i:i)
         endif
      enddo
      write(num,"(i10)") iat
      call cmlAddProperty(xf, units='unit:none', nrows=length1,
     &                    ref=trim(trim(mytag)//"."//adjustl(num)),
     &                    ncols=length2, value=array,
     &                    dictRef='nwchem:'//trim(key2))
      end subroutine

      subroutine ecceP2D_DP(key,mb_type,array,ld,length1,length2)
!
! Print 2D array of doubles
!
      integer ld,length1,length2,mb_type,i
      double precision array(ld,length2) 
      character(LEN=*) key
      character(LEN=80) key2
      if (ecce_print_enabled.eq.0) return
      key2=''
      do i=1,len_trim(key)
         if (key(i:i).eq.' ') then
            key2(i:i)='_'
         else
            key2(i:i)=key(i:i)
         endif
      enddo
      call cmlAddProperty(xf, units='unit:none', nrows=length1, 
     &                    ncols=length2, value=array, 
     &                    dictRef='nwchem:'//trim(key2))
      end subroutine

      subroutine ecceP2D_DPRef(key,mb_type,array,ld,length1,length2,
     &                          iat,mytag)
!
! Print 2D array of doubles
!
      integer ld,length1,length2,mb_type,i,iat
      double precision array(ld,length2)
      character(LEN=*) key,mytag
      character(LEN=80) key2
      character(LEN=10) num
      if (ecce_print_enabled.eq.0) return
      key2=''
      do i=1,len_trim(key)
         if (key(i:i).eq.' ') then
            key2(i:i)='_'
         else
            key2(i:i)=key(i:i)
         endif
      enddo
      write(num,"(i10)") iat
      call cmlAddProperty(xf, units='unit:none', nrows=length1,
     &                    ref=trim(trim(mytag)//"."//adjustl(num)),
     &                    ncols=length2, value=array,
     &                    dictRef='nwchem:'//trim(key2))
      end subroutine

      subroutine ecceP2D_Int3D(key,mb_type,array,ld,length1,length2)
!
! Print 2D array of ints
!
      integer ld,length1,length2,mb_type,i
      integer array(ld,length2,1)
      character(LEN=*) key
      character(LEN=80) key2
      if (ecce_print_enabled.eq.0) return
      key2=''
      do i=1,len_trim(key)
         if (key(i:i).eq.' ') then
            key2(i:i)='_'
         else
            key2(i:i)=key(i:i)
         endif
      enddo
      call cmlAddProperty(xf, units='unit:none', nrows=length1, 
     &                    ncols=length2, value=array(1:ld,1:length2,1),
     &                    dictRef='nwchem:'//trim(key2))
      end subroutine

      subroutine ecceP2D_Int3DRef(key,mb_type,array,ld,length1,length2,
     &                           iat,mytag)
!
! Print 2D array of ints
!
      integer ld,length1,length2,mb_type,i,iat
      integer array(ld,length2,1)
      character(LEN=*) key,mytag
      character(LEN=80) key2
      character(LEN=10) num
      if (ecce_print_enabled.eq.0) return
      key2=''
      do i=1,len_trim(key)
         if (key(i:i).eq.' ') then
            key2(i:i)='_'
         else
            key2(i:i)=key(i:i)
         endif
      enddo
      write(num,"(i10)") iat
      call cmlAddProperty(xf, units='unit:none', nrows=length1,
     &                    ref=trim(trim(mytag)//"."//adjustl(num)),
     &                    ncols=length2, value=array(1:ld,1:length2,1),
     &                    dictRef='nwchem:'//trim(key2))
      end subroutine


      subroutine ecceP2D_DP3D(key,mb_type,array,ld,length1,length2)
!
! Print 2D array of doubles
!
      integer ld,length1,length2,mb_type,i,iat
      double precision array(ld,length2,1)
      character(LEN=*) key
      character(LEN=80) key2
      if (ecce_print_enabled.eq.0) return
      key2=''
      do i=1,len_trim(key)
         if (key(i:i).eq.' ') then
            key2(i:i)='_'
         else
            key2(i:i)=key(i:i)
         endif
      enddo
      call cmlAddProperty(xf, units='unit:none', nrows=length1,
     &                    ncols=length2, value=array(1:ld,1:length2,1),
     &                    dictRef='nwchem:'//trim(key2))
      end subroutine

      subroutine ecceP2D_DP3DRef(key,mb_type,array,ld,length1,length2,
     &                             iat,mytag)
!
! Print 2D array of doubles
!
      integer ld,length1,length2,mb_type,i,iat
      double precision array(ld,length2,1)
      character(LEN=*) key,mytag
      character(LEN=80) key2
      character(LEN=10) num
      if (ecce_print_enabled.eq.0) return
      key2=''
      do i=1,len_trim(key)
         if (key(i:i).eq.' ') then
            key2(i:i)='_'
         else
            key2(i:i)=key(i:i)
         endif
      enddo
      write(num,"(i10)") iat
      call cmlAddProperty(xf, units='unit:none', nrows=length1,
     &                    ref=trim(trim(mytag)//"."//adjustl(num)),
     &                    ncols=length2, value=array(1:ld,1:length2,1),
     &                    dictRef='nwchem:'//trim(key2))
      end subroutine

      subroutine ecceP1D_Int(key,mb_type,array,length)
!
! Print 1D array of integers
!
      integer length,mb_type,i
      integer array(length)
      character(LEN=*) key
      character(LEN=80) key2
      if (ecce_print_enabled.eq.0) return
      key2=''
      do i=1,len_trim(key)
         if (key(i:i).eq.' ') then
            key2(i:i)='_'
         else
            key2(i:i)=key(i:i)
         endif
      enddo
      call cmlAddProperty(xf, value=array,nitems=length,
     &                  units='unit:none',dictRef='nwchem:'//trim(key2))
      end subroutine

      subroutine ecceP1D_IntRef(key,mb_type,array,length,iat,mytag)
!
! Print 1D array of integers
!
      integer length,mb_type,i,iat
      integer array(length)
      character(LEN=*) key,mytag
      character(LEN=80) key2
      character(LEN=10) num
      if (ecce_print_enabled.eq.0) return
      key2=''
      do i=1,len_trim(key)
         if (key(i:i).eq.' ') then
            key2(i:i)='_'
         else
            key2(i:i)=key(i:i)
         endif
      enddo
      write(num,"(i10)") iat
      call cmlAddProperty(xf, value=array,nitems=length,
     &                  ref=trim(trim(mytag)//"."//adjustl(num)),
     &                  units='unit:none',dictRef='nwchem:'//trim(key2))
      end subroutine

      subroutine ecceP1D_DP(key,mb_type,array,length)
!
! Print 1D array of doubles
!
      integer length,mb_type,i
      double precision array(length)  
      character(LEN=*) key
      character(LEN=80) key2
      if (ecce_print_enabled.eq.0) return
      key2=''
      do i=1,len_trim(key)
         if (key(i:i).eq.' ') then
            key2(i:i)='_'
         else
            key2(i:i)=key(i:i)
         endif
      enddo
      call cmlAddProperty(xf, value=array,nitems=length,
     &                  units='unit:none',dictRef='nwchem:'//trim(key2))
      end  subroutine

      subroutine ecceP1D_DPRef(key,mb_type,array,length,iat,mytag)
!
! Print 1D array of doubles
!
      integer length,mb_type,i,iat
      double precision array(length)
      character(LEN=*) key,mytag
      character(LEN=80) key2
      character(LEN=10) num
      if (ecce_print_enabled.eq.0) return
      key2=''
      do i=1,len_trim(key)
         if (key(i:i).eq.' ') then
            key2(i:i)='_'
         else
            key2(i:i)=key(i:i)
         endif
      enddo
      write(num,"(i10)") iat
      call cmlAddProperty(xf, value=array,nitems=length,
     &                  ref=trim(trim(mytag)//"."//adjustl(num)),
     &                  units='unit:none',dictRef='nwchem:'//trim(key2))
      end  subroutine

      subroutine ecceP1D_IntSc(key,mb_type,array)
!
! Print 1D integer
!
      integer length,mb_type,i
      integer array
      character(LEN=*) key
      character(LEN=80) key2
      if (ecce_print_enabled.eq.0) return
      key2=''
      do i=1,len_trim(key)
         if (key(i:i).eq.' ') then
            key2(i:i)='_'
         else
            key2(i:i)=key(i:i)
         endif
      enddo
      call cmlAddProperty(xf, value=array,
     &           units='unit:none', dictRef='nwchem:'//trim(key2))
      end subroutine

      subroutine ecceP1D_IntScRef(key,mb_type,array,iat,mytag)
!
! Print 1D integer
!
      integer length,mb_type,i,iat
      integer array
      character(LEN=*) key,mytag
      character(LEN=80) key2
      character(LEN=10) num
      if (ecce_print_enabled.eq.0) return
      key2=''
      do i=1,len_trim(key)
         if (key(i:i).eq.' ') then
            key2(i:i)='_'
         else
            key2(i:i)=key(i:i)
         endif
      enddo
      write(num,"(i10)") iat
      call cmlAddProperty(xf, value=array,
     &           ref=trim(trim(mytag)//"."//adjustl(num)),
     &           units='unit:none', dictRef='nwchem:'//trim(key2))
      end subroutine

      subroutine ecceP1D_DPSc(key, mb_type, array)
!
! Print 1D double
!
      integer length,mb_type,i
      double precision array
      character(LEN=*) key
      character(LEN=80) key2
      if (ecce_print_enabled.eq.0) return
      key2=''
      do i=1,len_trim(key)
         if (key(i:i).eq.' ') then
            key2(i:i)='_'
         else
            key2(i:i)=key(i:i)
         endif
      enddo
      call cmlAddProperty(xf, value=array,
     &           units='unit:none', dictRef='nwchem:'//trim(key2))
      end  subroutine

      subroutine ecceP1D_DPScRef(key, mb_type, array,iat,mytag)
!
! Print 1D double
!
      integer length,mb_type,i,iat
      double precision array
      character(LEN=*) key,mytag
      character(LEN=80) key2
      character(LEN=10) num
      if (ecce_print_enabled.eq.0) return
      key2=''
      do i=1,len_trim(key)
         if (key(i:i).eq.' ') then
            key2(i:i)='_'
         else
            key2(i:i)=key(i:i)
         endif
      enddo
      write(num,"(i10)") iat
      call cmlAddProperty(xf, value=array,
     &           ref=trim(trim(mytag)//"."//adjustl(num)),
     &           units='unit:none', dictRef='nwchem:'//trim(key2))
      end  subroutine

      subroutine eccePChar_Scalar2(key, string)
!
! Print character string

      integer length,i
      character(LEN=*) key
      character(LEN=80) key2
      character*2 string
      if (ecce_print_enabled.eq.0) return
      key2=''
      do i=1,len_trim(key)
         if (key(i:i).eq.' ') then
            key2(i:i)='_'
         else
            key2(i:i)=key(i:i)
         endif
      enddo
      call cmlAddProperty(xf, value=string,
     &           units='unit:none', dictRef='nwchem:'//trim(key2))
      end  subroutine

      subroutine eccePChar_Scalar2Ref(key,string,iat,mytag)
!
! Print character string
!
      integer length,i,iat
      character(LEN=*) key,mytag
      character(LEN=80) key2
      character*2 string
      character(LEN=10) num
      if (ecce_print_enabled.eq.0) return
      key2=''
      do i=1,len_trim(key)
         if (key(i:i).eq.' ') then
            key2(i:i)='_'
         else
            key2(i:i)=key(i:i)
         endif
      enddo
      write(num,"(i10)") iat
      call cmlAddProperty(xf, value=string,
     &           ref=trim(trim(mytag)//"."//adjustl(num)),
     &           units='unit:none', dictRef='nwchem:'//trim(key2))
      end  subroutine

      subroutine eccePChar_Array8(key, string, length)
!
! Print character string
!
      integer length,i
      character(LEN=*) key
      character(LEN=80) key2
      character*8 string(length)
      if (ecce_print_enabled.eq.0) return
      key2=''
      do i=1,len_trim(key)
         if (key(i:i).eq.' ') then
            key2(i:i)='_'
         else
            key2(i:i)=key(i:i)
         endif
      enddo
      call cmlAddProperty(xf, value=string,
     &                    nitems=length,units='unit:none',
     &                    dictRef='nwchem:'//trim(key2)) 
      end  subroutine

      subroutine eccePChar_Array8Ref(key,string,length,iat,mytag)
!
! Print character string
!
      integer length,i,iat
      character(LEN=*) key,mytag
      character(LEN=80) key2
      character*8 string(length)
      character(LEN=10) num
      if (ecce_print_enabled.eq.0) return
      key2=''
      do i=1,len_trim(key)
         if (key(i:i).eq.' ') then
            key2(i:i)='_'
         else
            key2(i:i)=key(i:i)
         endif
      enddo
      write(num,"(i10)") iat
      call cmlAddProperty(xf, value=string,
     &           ref=trim(trim(mytag)//"."//adjustl(num)),
     &                    nitems=length,units='unit:none',
     &                    dictRef='nwchem:'//trim(key2))
      end  subroutine

      subroutine ecce_print2_dbl_tol(key,array,ld,length1,length2,tol)
!
! Print 2D array of doubles with numbers smaller then tol set to zero
!
      integer ld,length1,length2,i,j
      double precision array(ld,length2),tol
      character(LEN=*) key
      character(LEN=80) key2
      if (ecce_print_enabled.eq.0) return
      key2=''
      do i=1,len_trim(key)
         if (key(i:i).eq.' ') then
            key2(i:i)='_'
         else
            key2(i:i)=key(i:i)
         endif
      enddo
      do i=1,length1
         do j=1,length2
            if (array(i,j).lt.tol) array(i,j)=0.0d0
         enddo
      enddo
      call cmlAddProperty(xf, units='unit:none',
     &  nrows=length1, ncols=length2,
     &  value=array, dictRef='nwchem:'//trim(key2))
      end subroutine
!
      subroutine ecce_print_file_open(filename)
! 
!     Open the CML file
!
      character(LEN=*) filename
      integer xx
      logical status,util_get_io_unit
      external util_get_io_unit
      status=util_get_io_unit(xx)
      if (ecce_print_enabled.eq.1) return
      call cmlBeginFile(xf, filename=trim(filename), unit=xx)
      call cmlAddNamespace(xf, prefix='compchem',
     &               URI='http://www.xml-cml.org/dictionary/compchem/')
      call cmlAddNamespace(xf, prefix='nwchem',
     &               URI='http://www.nwchem-sw.org/dictionary/nwchem/')
      call cmlStartCml(xf, convention='convention:compchem', 
     &                 validate=.true.)
      call cmlStartModule(xf, dictRef='compchem:jobList',
     &             title='NWChem simulation')
      ecce_print_enabled=1
      return
      end subroutine
!
      subroutine ecce_print_file_close()
! 
!     Close the CML file
!
      if (ecce_print_enabled.eq.0) return
      call cmlEndModule(xf)
      call cmlEndCml(xf)
      call cmlFinishFile(xf)
      ecce_print_enabled=0
      return
      end subroutine
!
      subroutine ecce_print_echo_input(input_file)
! 
!     Print NWChem input file into CML file
!
#include "stdio.fh"
      character(LEN=*) input_file
      character*255 this_line
      integer ios
      logical status,util_get_io_unit
      external util_get_io_unit
      if (ecce_print_enabled.eq.0) return
      call wcmlStartDec(xf, trim(input_file), dictRef='nwchem:Input')
      rewind(LuIn)
   1  read(LuIn, '(a)' , end=3, err=2) this_line
      call wcmlAddDecLine(xf, trim(this_line))
      goto 1
   2  call errquit('Error read file in ecce_print_echo_input',0,0)
   3  call wcmlEndDecList(xf)
      rewind(LuIn)
      return
      end subroutine
!
      subroutine ecce_print_module_entry(mymodule)
!
! Open module loop
!
      integer i
      character(LEN=*) mymodule
      character(LEN=80) key2
      if (ecce_print_enabled.eq.0) return
      key2=''
      do i=1,len_trim(mymodule)
         if (mymodule(i:i).eq.' ') then
            key2(i:i)='_'
         else
            key2(i:i)=mymodule(i:i)
         endif
      enddo
      call cmlStartModule(xf, title=trim(key2),
     &     dictref='nwchem:'//trim(key2))
      return
      end subroutine
!
      subroutine ecce_print_module_exit(mymodule,mystatus)
!
! Close module loop
!
      integer i
      character(LEN=*) mymodule,mystatus
      character(LEN=80) key2
      if (ecce_print_enabled.eq.0) return
      key2=''
      do i=1,len_trim(mymodule)
         if (mymodule(i:i).eq.' ') then
            key2(i:i)='_'
         else
            key2(i:i)=mymodule(i:i)
         endif
      enddo
      call xml_AddComment(xf,trim(key2)//' status '//trim(mystatus))
      call xml_AddComment(xf,'closing module '//trim(key2))
      call cmlEndModule(xf)
      return
      end subroutine
!
      subroutine ecce_proplist_entry(key,iat,tag)
      character(LEN=*) tag,key
      character(LEN=40) myref
      character(LEN=10) num
      integer iat
!
! Start writing property list
!
      if (ecce_print_enabled.eq.0) return
      write(num,"(i10)") iat
      myref=trim(tag)//"."//adjustl(num)
      call cmlStartPropertyList(xf,dictref='nwchem:'//trim(key),
     &                          ref=trim(myref))
      return
      end subroutine
!
      subroutine ecce_proplist_exit()
!
! Close writing property list
!
      if (ecce_print_enabled.eq.0) return
      call cmlEndPropertyList(xf)
      return
      end subroutine
!
      subroutine ecce_print_echo_string(mystring)
!
! Write string into CML file
!
      character(LEN=*) mystring
      if (ecce_print_enabled.eq.0) return
      call xml_AddComment(xf, trim(mystring))
      return
      end subroutine
!
      subroutine ecce_print_version(mystring)
!
! Write version of NWChem and date into metadata list CML file
!
#include "global.fh"
      character(LEN=*) mystring
      character*26 mydate
      integer*8 nproc
      if (ecce_print_enabled.eq.0) return
      call cmlStartModule(xf, title='NWChem runtime', 
     &                    dictref='compchem:environment')
      call cmlStartPropertyList(xf)
      call cmlAddProperty(xf, dictref='compchem:program',
     &                    value='NWChem')
      call cmlAddProperty(xf, dictref='compchem:programVersion', 
     &                    value='6.1')
      call util_date(mydate)
      mydate(25:26) = ' '
      call cmlAddProperty(xf, dictref='compchem:runDate',
     &                    value=trim(mydate))
      nproc = ga_nnodes()  
      call cmlAddProperty(xf, dictref='compchem:numProc',
     &                    value=nproc, units='unit:none')
      call cmlEndPropertyList(xf)
      call cmlEndModule(xf)
      return
      end subroutine
!
      logical function is_ecce_print_on()
! 
!     Returns true if ecce print is turned on
!
      is_ecce_print_on=ecce_print_enabled.eq.1
      return
      end function   
!
      subroutine ecce_print_control(new,old)
! 
!     Toggle to print to CML file on and off
!
      integer new,old
      old = 0
      if (ecce_print_enabled.eq.1) old = 1
      ecce_print_enabled = 0
      if (new.eq.1) ecce_print_enabled = 1
      return
      end subroutine
!
      end module
