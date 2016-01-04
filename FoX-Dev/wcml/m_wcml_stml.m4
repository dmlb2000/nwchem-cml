dnl
include(`foreach.m4')dnl
dnl
include(`common.m4')dnl
dnl
include(`datatypes.m4')dnl
dnl
define(`TOHWM4_splitlines', `dnl
dnl Cannot for the life of me do this splitting correctly in m4 alone.
esyscmd(`echo "'$1`" | awk \{i=1\;while\(i\<\(\length\(\)-132\)\)\{print\ \substr\(\$\0,i,131\)\"\&\"\;i+=131\}print\ substr\(\$\0,i,132\)\} -') dnl
') dnl
dnl
dnl
define(`TOHWM4_subroutinename', `stmAdd$1')dnl
define(`TOHWM4_interfacename',`module procedure TOHWM4_subroutinename(`$1',`$2',`$3',`$4')')dnl
dnl
define(`TOHWM4_interfacelist', `dnl
     module procedure stmAdd$1$2
')dnl
dnl
define(`TOHWM4_dummyargcall', `,`$1'=`$1'')dnl
dnl
define(`TOHWM4_stml_sub',`dnl
  subroutine stmAdd$1$3(xf, value, &
TOHWM4_dummyarglist($2) &
ifelse(`$1', `Lg', `', `, units')`'dnl
ifelse(`$1', `Ch', `, dataType ifelse(`$3', `Sca', `', `, delimiter')')`'dnl
ifelse(substr($1,0,4), `Real', `, fmt', substr($1,0,5), `Cmplx', `, fmt')`'dnl
)

    type(xmlf_t), intent(inout) :: xf
    TOHWM4_declarationtype($1), intent(in) dnl
ifelse(`$3', `Arr', `, dimension(:)', `$3', `Mat', `, dimension(:,:)') dnl 
:: value
m4_foreach(`x', `$2', `TOHWM4_dummyargdecl(x)')
ifelse(`$1', `Lg', `', `dnl
TOHWM4_dummyargdecl(units)
')dnl
ifelse(`$1', `Ch', `dnl
TOHWM4_dummyargdecl(dataType)
ifelse(`$3', `Sca', `', `dnl
    character(len=1), intent(in), optional :: delimiter
')dnl
')dnl
ifelse(substr($1,0,4),`Real',`TOHWM4_dummyargdecl(fmt)',substr($1,0,5),`Cmplx',`TOHWM4_dummyargdecl(fmt)')dnl
ifelse(`$3', `Sca', `dnl
    call xml_NewElement(xf, "scalar")
', `$3', `Arr', `dnl
    call xml_NewElement(xf, "array")
    call xml_AddAttribute(xf, "size", size(value))
', `$3', `Mat', `dnl
    call xml_NewElement(xf, "matrix")
    call xml_AddAttribute(xf, "rows", size(value,1))
    call xml_AddAttribute(xf, "columns", size(value,2))
')dnl
dnl
ifelse(`$1', `Ch', `dnl
ifelse(`$3', `Sca', `', `dnl
    if (present(delimiter)) then
      call xml_AddAttribute(xf, "delimiter", delimiter)
    else
      call xml_AddAttribute(xf, "delimiter", " ")
    endif
')dnl
    if (present(dataType)) then
      call xml_addAttribute(xf, "dataType", dataType)
    else
')dnl
    call xml_AddAttribute(xf, "dataType", dnl
"TOHWM4_datatype(`$1')")
ifelse(`$1', `Ch', `    endif')

m4_foreach(`x', $2, `TOHWM4_dummyarguse(x)')
ifelse(`$1', `Lg', `', `dnl
TOHWM4_dummyarguse(units)
')dnl
    call xml_AddCharacters(xf, value`'dnl
ifelse(substr($1,0,4), `Real', `, fmt)', substr($1,0,5), `Cmplx', `, fmt)',
`$1', `Ch', `ifelse(`$3', `Sca', `)', `, delimiter)')', `)')

ifelse(`$3', `Sca', `dnl
    call xml_EndElement(xf, "scalar")
', `$3', `Arr', `dnl
    call xml_EndElement(xf, "array")
', `$3', `Mat', `dnl
    call xml_EndElement(xf, "matrix")
')

  end subroutine stmAdd$1$3
')dnl
dnl
! This file is AUTOGENERATED!!!!
! Do not edit this file; edit m_wcml_stml.m4 and regenerate.
!
!
module m_wcml_stml

#ifndef DUMMYLIB
  use FoX_wxml, only: xmlf_t
  use FoX_wxml, only: xml_NewElement, xml_EndElement
  use FoX_wxml, only: xml_AddCharacters, xml_AddAttribute

! Fix for pgi, requires this explicitly:
  use m_wxml_overloads

  implicit none
  private

  integer, private, parameter ::  sp = selected_real_kind(6,30)
  integer, private, parameter ::  dp = selected_real_kind(14,100)

  interface stmAddValue
m4_foreach(`x', TOHWM4_types, `TOHWM4_interfacelist(x, `Sca')')
m4_foreach(`x', TOHWM4_types, `TOHWM4_interfacelist(x, `Arr')')
m4_foreach(`x', TOHWM4_types, `TOHWM4_interfacelist(x, `Mat')')
  end interface stmAddValue

  interface stmAddScalar
m4_foreach(`x', TOHWM4_types, `TOHWM4_interfacelist(x, `Sca')')
  end interface stmAddScalar

  interface stmAddArray
m4_foreach(`x', TOHWM4_types, `TOHWM4_interfacelist(x, `Arr')')
  end interface stmAddArray

  interface stmAddMatrix
m4_foreach(`x', TOHWM4_types, `TOHWM4_interfacelist(x, `Mat')')
  end interface stmAddMatrix

  public :: stmAddValue
  public :: stmAddScalar
  public :: stmAddArray
  public :: stmAddMatrix

contains

m4_foreach(`x', TOHWM4_types, `TOHWM4_stml_sub(x,`(id, title, dictRef, convention, errorValue, errorBasis, min, max, ref)',`Sca')
')
m4_foreach(`x', TOHWM4_types, `TOHWM4_stml_sub(x,`(id, title, dictRef, convention, errorValue, errorBasis, min, max, ref)',`Arr')
')
m4_foreach(`x', TOHWM4_types, `TOHWM4_stml_sub(x,`(id, title, dictRef, convention, errorValue, errorBasis, min, max, ref)',`Mat')
')
#endif
end module m_wcml_stml
