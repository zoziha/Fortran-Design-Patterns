program wrapper_main

    use wrapper_module, only: vegge_mania_type, cheese_topping_type, tomato_topping_type
    implicit none
    type(vegge_mania_type), target :: pizza
    type(cheese_topping_type), target :: pizza_with_cheese
    type(tomato_topping_type) :: pizza_with_tomato_and_cheese

    pizza_with_cheese%pizza => pizza
    pizza_with_tomato_and_cheese%pizza => pizza_with_cheese

    print *, "Prince of veggeMania with tomato and cheese topping is ", pizza_with_tomato_and_cheese%get_price()

end program wrapper_main

!> Results shall be:

! Prince of veggeMania with tomato and cheese topping is           32.
