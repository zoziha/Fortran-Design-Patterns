!> @note use `select type` is a limited form of polymorphism
program interface_specific_main

    use interface_specific_module, only: shape_type, circle_type, print_circle
    implicit none
    class(shape_type), allocatable :: s1

    allocate (circle_type :: s1)

    select type (s1)
    type is (circle_type)
        call print_circle(s1)
    end select

end program interface_specific_main
