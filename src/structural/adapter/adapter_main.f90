program adapter_main
    use adapter_module, only: client_t, computer_t, mac_t, windows_t, windows_adapter_t
    implicit none
    type(client_t) :: client
    type(mac_t) :: mac
    type(windows_t), target :: windows
    type(windows_adapter_t) :: windows_adapter

    call client%insert_lightning_connector_into_computer(mac)
    windows_adapter%windows_machine => windows
    call client%insert_lightning_connector_into_computer(windows_adapter)

end program adapter_main

!> Results shall be:

!  Client inserts Lightning connector into computer.
!  Lightning connector is plugged into mac machine.
!  Client inserts Lightning connector into computer.
!  Adapter converts Lightning signal to USB.
!  USB connector is plugged into windows machine.