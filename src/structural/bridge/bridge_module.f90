module bridge_module

    implicit none
    private

    public :: hp_type, epson_type, mac_type, windows_type

    type, abstract :: computer_type
    contains
        procedure(computer_type_print), deferred :: print
        procedure(computer_type_set_printer), deferred :: set_printer
    end type computer_type

    type, abstract :: printer_type
    contains
        procedure(printer_type_print_file), deferred :: print_file
    end type printer_type

    abstract interface

        subroutine computer_type_print(self)
            import computer_type
            class(computer_type), intent(inout) :: self
        end subroutine computer_type_print

        subroutine computer_type_set_printer(self, printer)
            import computer_type, printer_type
            class(computer_type), intent(inout) :: self
            class(printer_type), intent(inout), target :: printer
        end subroutine computer_type_set_printer

        subroutine printer_type_print_file(self)
            import printer_type
            class(printer_type), intent(inout) :: self
        end subroutine printer_type_print_file

    end interface

    type, extends(printer_type) :: epson_type
    contains
        procedure :: print_file => epson_type_print_file
    end type epson_type

    type, extends(printer_type) :: hp_type
    contains
        procedure :: print_file => hp_type_print_file
    end type hp_type

    type, extends(computer_type) :: mac_type
        class(printer_type), pointer :: printer
    contains
        procedure :: print => mac_type_print
        procedure :: set_printer => mac_type_set_printer
    end type mac_type

    type, extends(computer_type) :: windows_type
        class(printer_type), pointer :: printer
    contains
        procedure :: print => windows_type_print
        procedure :: set_printer => windows_type_set_printer
    end type windows_type

contains

    subroutine windows_type_print(self)
        class(windows_type), intent(inout) :: self
        print *, "Print request for windows"
        call self%printer%print_file()
    end subroutine windows_type_print

    subroutine windows_type_set_printer(self, printer)
        class(windows_type), intent(inout) :: self
        class(printer_type), intent(inout), target :: printer
        self%printer => printer
    end subroutine windows_type_set_printer

    subroutine mac_type_print(self)
        class(mac_type), intent(inout) :: self
        print *, "Print request for mac"
        call self%printer%print_file()
    end subroutine mac_type_print

    subroutine mac_type_set_printer(self, printer)
        class(mac_type), intent(inout) :: self
        class(printer_type), intent(inout), target :: printer
        self%printer => printer
    end subroutine mac_type_set_printer

    subroutine epson_type_print_file(self)
        class(epson_type), intent(inout) :: self
        print *, "Printing by a EPSON Printer"
    end subroutine epson_type_print_file

    subroutine hp_type_print_file(self)
        class(hp_type), intent(inout) :: self
        print *, "Printing by a HP Printer"
    end subroutine hp_type_print_file

end module bridge_module
