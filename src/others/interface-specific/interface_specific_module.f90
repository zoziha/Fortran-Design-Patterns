module interface_specific_module

    implicit none

    private
    public :: shape, circle, print_circle

    type, abstract :: shape
    end type shape

    type, extends(shape) :: circle
    end type circle

contains

    !> print circle
    subroutine print_circle(this)
        type(circle), intent(in) :: this

        print *, 'circle'

    end subroutine print_circle

end module interface_specific_module
