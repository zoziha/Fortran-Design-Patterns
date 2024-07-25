program adapter_main
    use adapter_module, only: client_type, computer_type, mac_type, windows_type, windows_adapter_type
    implicit none
    type(client_type) :: client
    type(mac_type) :: mac
    type(windows_type), target :: windows
    type(windows_adapter_type) :: windows_adapter

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