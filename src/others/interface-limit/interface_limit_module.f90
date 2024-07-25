!> @note Because fortran's interfaces are static, it is difficult to implement dynamic binding,
!> which restricts abstract classes from being fully functional or interacting with each other in a way that is not flexible;
!> instead, it would be more practical to use non-procedural binding.
module interface_limit_module

    implicit none

    private
    public :: circle_type, square_type, shape_type, cs_interact

    type, abstract :: shape_type
    end type shape_type

    abstract interface
        subroutine interact(shape1, shape2)
            import :: shape_type
            class(shape_type), intent(inout) :: shape1, shape2
        end subroutine interact
    end interface

    type, extends(shape_type) :: circle_type
    end type circle_type

    type, extends(shape_type) :: square_type
    end type square_type

contains

    !> @note This is a non-procedural binding, which is more flexible than procedural binding when it have to comes to dynamic binding.
    subroutine cs_interact(cir, squ)
        type(circle_type), intent(inout) :: cir
        type(square_type), intent(inout) :: squ

        print *, "circle-square interaction"

    end subroutine cs_interact

end module interface_limit_module
