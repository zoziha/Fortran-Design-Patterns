module interface_specific_module

    implicit none

    private
    public :: shape_type, circle_type, print_circle

    type, abstract :: shape_type
    end type shape_type

    type, extends(shape_type) :: circle_type
    end type circle_type

contains

    !> print circle
    subroutine print_circle(this)
        type(circle_type), intent(in) :: this

        print *, 'circle'

    end subroutine print_circle

end module interface_specific_module
