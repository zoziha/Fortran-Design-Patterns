module adapter_module

    implicit none
    private

    public :: client_type, computer_type, mac_type, windows_type, windows_adapter_type

    type client_type
    contains
        procedure :: insert_lightning_connector_into_computer => client_type_insert_lightning_connector_into_computer
    end type client_type

    type, abstract :: computer_type
    contains
        procedure(computer_type_insert_into_lightning_port), deferred :: insert_into_lightning_port
    end type computer_type

    type, extends(computer_type) :: mac_type
    contains
        procedure :: insert_into_lightning_port => mac_type_insert_into_lightning_port
    end type mac_type

    type, extends(computer_type) :: windows_type
    contains
        procedure :: insert_into_lightning_port => windows_type_insert_into_lightning_port
    end type windows_type

    type, extends(computer_type) :: windows_adapter_type
        type(windows_type), pointer :: windows_machine
    contains
        procedure :: insert_into_lightning_port => windows_adapter_type_insert_into_lightning_port
    end type windows_adapter_type

    abstract interface
        subroutine computer_type_insert_into_lightning_port(self)
            import computer_type
            class(computer_type), intent(inout) :: self
        end subroutine computer_type_insert_into_lightning_port
    end interface

contains

    subroutine client_type_insert_lightning_connector_into_computer(self, com)
        class(client_type), intent(inout) :: self
        class(computer_type), intent(inout) :: com
        print *, "Client inserts Lightning connector into computer."
        call com%insert_into_lightning_port()
    end subroutine client_type_insert_lightning_connector_into_computer

    subroutine mac_type_insert_into_lightning_port(self)
        class(mac_type), intent(inout) :: self
        print *, "Lightning connector is plugged into mac machine."
    end subroutine mac_type_insert_into_lightning_port

    subroutine windows_type_insert_into_lightning_port(self)
        class(windows_type), intent(inout) :: self
        print *, "USB connector is plugged into windows machine."
    end subroutine windows_type_insert_into_lightning_port

    subroutine windows_adapter_type_insert_into_lightning_port(self)
        class(windows_adapter_type), intent(inout) :: self
        print *, "Adapter converts Lightning signal to USB."
        call self%windows_machine%insert_into_lightning_port()
    end subroutine windows_adapter_type_insert_into_lightning_port

end module adapter_module
