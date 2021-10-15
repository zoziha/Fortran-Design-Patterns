!> Reference: https://refactoring.guru/design-patterns/command/go/example
module command_pattern
    
    implicit none
    private

    public :: tv, on_command, off_command, button

    !> Abstract classes

    type, abstract :: command
    contains
        procedure(execute_procedure), deferred :: execute
    end type command

    type, abstract :: device
    contains
        procedure(on_procedure), deferred :: on
        procedure(off_procedure), deferred :: off
    end type device

    abstract interface
        subroutine execute_procedure(self)
            import command
            class(command), intent(inout) :: self
        end subroutine execute_procedure
        subroutine on_procedure(self)
            import device
            class(device), intent(inout) :: self
        end subroutine on_procedure
        subroutine off_procedure(self)
            import device
            class(device), intent(inout) :: self
        end subroutine off_procedure
    end interface

    !> Specific Objects

    type, extends(command) :: on_command
        class(device), pointer :: d
    contains
        procedure :: execute => on_command_execute
    end type on_command 

    type, extends(command) :: off_command
        class(device), pointer :: d
    contains
        procedure :: execute => off_command_execute
    end type off_command

    type, extends(device) :: tv
        logical :: is_running
    contains
        procedure :: on => tv_on
        procedure :: off => tv_off
    end type tv

    type :: button
        class(command), pointer :: c
    contains
        procedure :: press
    end type button

contains

    subroutine press(self)
        class(button), intent(inout) :: self
        call self%c%execute()
    end subroutine press

    subroutine on_command_execute(self)
        class(on_command), intent(inout) :: self
        call self%d%on()
    end subroutine on_command_execute

    subroutine off_command_execute(self)
        class(off_command), intent(inout) :: self
        call self%d%off()
    end subroutine off_command_execute

    subroutine tv_on(self)
        class(tv), intent(inout) :: self
        self%is_running = .true.
        print *, "Turning tv on. ✔️"
    end subroutine tv_on

    subroutine tv_off(self)
        class(tv), intent(inout) :: self
        self%is_running = .false.
        print *, "Turning tv off. ❌"
    end subroutine tv_off

end module command_pattern