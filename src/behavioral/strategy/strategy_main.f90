program strategy_main

    use strategy_module, only: add_t, sub_t, calculator_t
    implicit none
    type(add_t) :: add
    type(sub_t) :: sub
    type(calculator_t) :: calculator

    call calculator%set_strategy(add)
    print *, "Add:", calculator%strategy%calc(1, 1)

    call calculator%set_strategy(sub)
    print *, "Sub:", calculator%strategy%calc(1, 1)

end program strategy_main

!> Results shall be:

!  Add:           2
!  Sub:           0