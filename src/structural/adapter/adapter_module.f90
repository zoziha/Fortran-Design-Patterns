module adapter_module

    implicit none
    private

    public :: client_t, computer_t, mac_t, windows_t, windows_adapter_t

    type client_t
    contains
        procedure :: insert_lightning_connector_into_computer => client_t_insert_lightning_connector_into_computer
    end type client_t

    type, abstract :: computer_t
    contains
        procedure(computer_t_insert_into_lightning_port), deferred :: insert_into_lightning_port
    end type computer_t

    type, extends(computer_t) :: mac_t
    contains
        procedure :: insert_into_lightning_port => mac_t_insert_into_lightning_port
    end type mac_t

    type, extends(computer_t) :: windows_t
    contains
        procedure :: insert_into_lightning_port => windows_t_insert_into_lightning_port
    end type windows_t

    type, extends(computer_t) :: windows_adapter_t
        type(windows_t), pointer :: windows_machine
    contains
        procedure :: insert_into_lightning_port => windows_adapter_t_insert_into_lightning_port
    end type windows_adapter_t

    abstract interface
        subroutine computer_t_insert_into_lightning_port(self)
            import computer_t
            class(computer_t), intent(inout) :: self
        end subroutine computer_t_insert_into_lightning_port
    end interface

contains

    subroutine client_t_insert_lightning_connector_into_computer(self, com)
        class(client_t), intent(inout) :: self
        class(computer_t), intent(inout) :: com
        print *, "Client inserts Lightning connector into computer."
        call com%insert_into_lightning_port()
    end subroutine client_t_insert_lightning_connector_into_computer

    subroutine mac_t_insert_into_lightning_port(self)
        class(mac_t), intent(inout) :: self
        print *, "Lightning connector is plugged into mac machine."
    end subroutine mac_t_insert_into_lightning_port

    subroutine windows_t_insert_into_lightning_port(self)
        class(windows_t), intent(inout) :: self
        print *, "USB connector is plugged into windows machine."
    end subroutine windows_t_insert_into_lightning_port

    subroutine windows_adapter_t_insert_into_lightning_port(self)
        class(windows_adapter_t), intent(inout) :: self
        print *, "Adapter converts Lightning signal to USB."
        call self%windows_machine%insert_into_lightning_port()
    end subroutine windows_adapter_t_insert_into_lightning_port

end module adapter_module
