define(`TOHWM4_vibration_subs', `dnl
  subroutine cmlAddMolecularVibrations_$1(xf, natoms, frequencies, eigenmodes, intensities)
    type(xmlf_t), intent(inout) :: xf
    integer, intent(in) :: natoms            
    real(kind=$1), intent(in)              :: frequencies(3*natoms)
    real(kind=$1), intent(in)              :: eigenmodes(9*natoms*natoms)
    real(kind=$1), intent(in), optional    :: intensities(3*natoms)

#ifndef DUMMYLIB
    integer          :: i, ilo, ihi
    character(len=6) :: mon

    call cmlStartPropertyList(xf=xf, dictRef="molecularVibrations")
    do i = 1, 3*natoms
       write(mon,"(i6)") i
       call cmlStartPropertyList(xf=xf, dictRef="vibrationalMode", id=trim("mode."//adjustl(mon)))
       call stmAddValue(xf=xf, value=frequencies(i), dictRef="normalModeFrequency")
       if (present(intensities)) call stmAddValue(xf=xf, value=intensities(i), dictRef="normalModeInfraRedIntensity")
       ilo = (i-1)*3*natoms + 1
       ihi = ilo + 3*natoms
       call stmAddValue(xf=xf, value=eigenmodes(ilo:ihi), dictRef="normalMode")
       call cmlEndPropertyList(xf)
    enddo
    call cmlEndPropertyList(xf)
#endif

  end subroutine cmlAddMolecularVibrations_$1

')dnl
dnl

!
! This file is AUTOGENERATED
! To update, edit m_wcml_vibrations.m4 and regenerate

module m_wcml_vibrations

  use fox_m_fsys_realtypes, only: sp, dp
  use FoX_wxml, only: xmlf_t
#ifndef DUMMYLIB
  use m_wcml_lists, only: cmlStartPropertyList, cmlEndPropertyList
  use m_wcml_stml, only: stmAddValue

! Fix for pgi, requires this explicitly:
  use m_wxml_overloads
#endif

  implicit none
  private

  interface cmlAddMolecularVibrations
    module procedure cmlAddMolecularVibrations_sp
    module procedure cmlAddMolecularVibrations_dp
  end interface

  public :: cmlAddMolecularVibrations

contains

TOHWM4_vibration_subs(`sp')

TOHWM4_vibration_subs(`dp')

end module m_wcml_vibrations
