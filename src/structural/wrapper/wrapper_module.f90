module wrapper_module

    implicit none
    private

    public :: vegge_mania_t, tomato_topping_t, cheese_topping_t

    type, abstract :: pizza_t
    contains
        procedure(pizza_t_get_price), deferred :: get_price
    end type pizza_t

    abstract interface
        function pizza_t_get_price(self) result(price)
            import :: pizza_t
            class(pizza_t), intent(inout) :: self
            integer :: price
        end function pizza_t_get_price
    end interface

    type, extends(pizza_t) :: vegge_mania_t
    contains
        procedure :: get_price => vegge_mania_t_get_price
    end type vegge_mania_t

    type, extends(pizza_t) :: tomato_topping_t
        class(pizza_t), pointer :: pizza
    contains
        procedure :: get_price => tomato_topping_t_get_price
    end type tomato_topping_t

    type, extends(pizza_t) :: cheese_topping_t
        class(pizza_t), pointer :: pizza
    contains
        procedure :: get_price => cheese_topping_t_get_price
    end type cheese_topping_t

contains

    function vegge_mania_t_get_price(self) result(price)
        class(vegge_mania_t), intent(inout) :: self
        integer :: price
        price = 15
    end function vegge_mania_t_get_price

    function tomato_topping_t_get_price(self) result(price)
        class(tomato_topping_t), intent(inout) :: self
        integer :: price
        price = self%pizza%get_price() + 7
    end function tomato_topping_t_get_price

    function cheese_topping_t_get_price(self) result(price)
        class(cheese_topping_t), intent(inout) :: self
        integer :: price
        price = self%pizza%get_price() + 10
    end function cheese_topping_t_get_price

end module wrapper_module
