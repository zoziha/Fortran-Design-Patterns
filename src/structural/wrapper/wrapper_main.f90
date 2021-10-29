program wrapper_main

    use wrapper_module, only: vegge_mania_t, cheese_topping_t, tomato_topping_t
    implicit none
    type(vegge_mania_t), target :: pizza
    type(cheese_topping_t), target :: pizza_with_cheese
    type(tomato_topping_t) :: pizza_with_tomato_and_cheese

    pizza_with_cheese%pizza => pizza
    pizza_with_tomato_and_cheese%pizza => pizza_with_cheese

    print *, "Prince of veggeMania with tomato and cheese topping is ", pizza_with_tomato_and_cheese%get_price()

end program wrapper_main

!> Results shall be:

! Prince of veggeMania with tomato and cheese topping is           32.
