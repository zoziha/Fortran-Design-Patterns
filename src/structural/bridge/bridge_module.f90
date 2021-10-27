module bridge_module

    implicit none
    private

    public :: hp_t, epson_t, mac_t, windows_t

    type, abstract :: computer_t
    contains
        procedure(computer_t_print), deferred :: print
        procedure(computer_t_set_printer), deferred :: set_printer
    end type computer_t

    type, abstract :: printer_t
    contains
        procedure(printer_t_print_file), deferred :: print_file
    end type printer_t

    abstract interface

        subroutine computer_t_print(self)
            import computer_t
            class(computer_t), intent(inout) :: self
        end subroutine computer_t_print

        subroutine computer_t_set_printer(self, printer)
            import computer_t, printer_t
            class(computer_t), intent(inout) :: self
            class(printer_t), intent(inout), target :: printer
        end subroutine computer_t_set_printer

        subroutine printer_t_print_file(self)
            import printer_t
            class(printer_t), intent(inout) :: self
        end subroutine printer_t_print_file

    end interface

    type, extends(printer_t) :: epson_t
    contains
        procedure :: print_file => epson_t_print_file
    end type epson_t

    type, extends(printer_t) :: hp_t
    contains
        procedure :: print_file => hp_t_print_file
    end type hp_t

    type, extends(computer_t) :: mac_t
        class(printer_t), pointer :: printer
    contains
        procedure :: print => mac_t_print
        procedure :: set_printer => mac_t_set_printer
    end type mac_t

    type, extends(computer_t) :: windows_t
        class(printer_t), pointer :: printer
    contains
        procedure :: print => windows_t_print
        procedure :: set_printer => windows_t_set_printer
    end type windows_t

contains

    subroutine windows_t_print(self)
        class(windows_t), intent(inout) :: self
        print *, "Print request for windows"
        call self%printer%print_file()
    end subroutine windows_t_print

    subroutine windows_t_set_printer(self, printer)
        class(windows_t), intent(inout) :: self
        class(printer_t), intent(inout), target :: printer
        self%printer => printer
    end subroutine windows_t_set_printer

    subroutine mac_t_print(self)
        class(mac_t), intent(inout) :: self
        print *, "Print request for mac"
        call self%printer%print_file()
    end subroutine mac_t_print

    subroutine mac_t_set_printer(self, printer)
        class(mac_t), intent(inout) :: self
        class(printer_t), intent(inout), target :: printer
        self%printer => printer
    end subroutine mac_t_set_printer

    subroutine epson_t_print_file(self)
        class(epson_t), intent(inout) :: self
        print *, "Printing by a EPSON Printer"
    end subroutine epson_t_print_file

    subroutine hp_t_print_file(self)
        class(hp_t), intent(inout) :: self
        print *, "Printing by a HP Printer"
    end subroutine hp_t_print_file

end module bridge_module
