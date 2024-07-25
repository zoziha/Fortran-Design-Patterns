program strategy_main

    use strategy_module, only: add_type, sub_type, calculator_type
    implicit none
    type(add_type) :: add
    type(sub_type) :: sub
    type(calculator_type) :: calculator

    call calculator%set_strategy(add)
    print *, "Add:", calculator%strategy%calc(1, 1)

    call calculator%set_strategy(sub)
    print *, "Sub:", calculator%strategy%calc(1, 1)

end program strategy_main

!> Results shall be:

!  Add:           2
!  Sub:           0