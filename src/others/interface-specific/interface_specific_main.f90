!> @note use `select type` is a limited form of polymorphism
program interface_specific_main

    use interface_specific_module, only: shape, circle, print_circle
    implicit none
    class(shape), allocatable :: s1

    allocate (circle :: s1)

    select type (s1)
    type is (circle)
        call print_circle(s1)
    end select

end program interface_specific_main
