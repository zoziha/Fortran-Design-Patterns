program bridge_main

    use bridge_module, only: hp_t, epson_t, mac_t, windows_t
    implicit none
    
    type(hp_t) :: hp_printer
    type(epson_t) :: epson_printer
    type(mac_t) :: mac_computer
    type(windows_t) :: windows_computer
    
    call mac_computer%set_printer(hp_printer)
    call mac_computer%print()
    
    call mac_computer%set_printer(epson_printer)
    call mac_computer%print()
    
    call windows_computer%set_printer(hp_printer)
    call windows_computer%print()
    
    call windows_computer%set_printer(epson_printer)
    call windows_computer%print()

end program bridge_main

!> Results shall be:

!  Print request for mac
!  Printing by a HP Printer
!  Print request for mac
!  Printing by a EPSON Printer
!  Print request for windows
!  Printing by a HP Printer
!  Print request for windows
!  Printing by a EPSON Printer