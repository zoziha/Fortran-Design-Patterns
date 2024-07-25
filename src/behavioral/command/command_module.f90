!> Reference: https://refactoring.guru/design-patterns/command/go/example
module command_pattern

    implicit none
    private

    public :: tv_type, on_command_type, off_command_type, button_type

    !> Abstract classes

    type, abstract :: command_type
    contains
        procedure(execute_procedure), deferred :: execute
    end type command_type

    type, abstract :: device_type
    contains
        procedure(on_procedure), deferred :: on
        procedure(off_procedure), deferred :: off
    end type device_type

    abstract interface
        subroutine execute_procedure(self)
            import command_type
            class(command_type), intent(inout) :: self
        end subroutine execute_procedure
        subroutine on_procedure(self)
            import device_type
            class(device_type), intent(inout) :: self
        end subroutine on_procedure
        subroutine off_procedure(self)
            import device_type
            class(device_type), intent(inout) :: self
        end subroutine off_procedure
    end interface

    !> Specific Objects

    type, extends(command_type) :: on_command_type
        class(device_type), pointer :: d
    contains
        procedure :: execute => on_command_type_execute
    end type on_command_type

    type, extends(command_type) :: off_command_type
        class(device_type), pointer :: d
    contains
        procedure :: execute => off_command_type_execute
    end type off_command_type

    type, extends(device_type) :: tv_type
        logical :: is_running
    contains
        procedure :: on => tv_type_on
        procedure :: off => tv_type_off
    end type tv_type

    type :: button_type
        class(command_type), pointer :: c
    contains
        procedure :: press
    end type button_type

contains

    subroutine press(self)
        class(button_type), intent(inout) :: self
        call self%c%execute()
    end subroutine press

    subroutine on_command_type_execute(self)
        class(on_command_type), intent(inout) :: self
        call self%d%on()
    end subroutine on_command_type_execute

    subroutine off_command_type_execute(self)
        class(off_command_type), intent(inout) :: self
        call self%d%off()
    end subroutine off_command_type_execute

    subroutine tv_type_on(self)
        class(tv_type), intent(inout) :: self
        self%is_running = .true.
        print *, "Turning tv on. ✔️"
    end subroutine tv_type_on

    subroutine tv_type_off(self)
        class(tv_type), intent(inout) :: self
        self%is_running = .false.
        print *, "Turning tv off. ❌"
    end subroutine tv_type_off

end module command_pattern
