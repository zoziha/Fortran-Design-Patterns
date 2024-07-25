module wrapper_module

    implicit none
    private

    public :: vegge_mania_type, tomato_topping_type, cheese_topping_type

    type, abstract :: pizza_type
    contains
        procedure(pizza_t_get_price), deferred :: get_price
    end type pizza_type

    abstract interface
        function pizza_t_get_price(self) result(price)
            import :: pizza_type
            class(pizza_type), intent(inout) :: self
            integer :: price
        end function pizza_t_get_price
    end interface

    type, extends(pizza_type) :: vegge_mania_type
    contains
        procedure :: get_price => vegge_mania_type_get_price
    end type vegge_mania_type

    type, extends(pizza_type) :: tomato_topping_type
        class(pizza_type), pointer :: pizza
    contains
        procedure :: get_price => tomato_topping_type_get_price
    end type tomato_topping_type

    type, extends(pizza_type) :: cheese_topping_type
        class(pizza_type), pointer :: pizza
    contains
        procedure :: get_price => cheese_topping_type_get_price
    end type cheese_topping_type

contains

    function vegge_mania_type_get_price(self) result(price)
        class(vegge_mania_type), intent(inout) :: self
        integer :: price
        price = 15
    end function vegge_mania_type_get_price

    function tomato_topping_type_get_price(self) result(price)
        class(tomato_topping_type), intent(inout) :: self
        integer :: price
        price = self%pizza%get_price() + 7
    end function tomato_topping_type_get_price

    function cheese_topping_type_get_price(self) result(price)
        class(cheese_topping_type), intent(inout) :: self
        integer :: price
        price = self%pizza%get_price() + 10
    end function cheese_topping_type_get_price

end module wrapper_module
