program interface_limit_main

    use interface_limit_module, only: circle, square, cs_interact
    implicit none
    type(circle) :: c1
    type(square) :: s1

    call cs_interact(c1, s1)

end program interface_limit_main
