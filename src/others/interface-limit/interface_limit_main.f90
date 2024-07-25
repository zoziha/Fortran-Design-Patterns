program interface_limit_main

    use interface_limit_module, only: circle_type, square_type, cs_interact
    implicit none
    type(circle_type) :: c1
    type(square_type) :: s1

    call cs_interact(c1, s1)

end program interface_limit_main
