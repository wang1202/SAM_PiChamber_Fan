subroutine setdetector()
! Initialize all detectors in pi chamber
! FY @ MTU 2017
use vars
use params

call setdetector_rtd()

call setdetector_sonic()

call setdetector_licor()

call setdetector_thermistors()

call setdetector_virtual()

end subroutine
