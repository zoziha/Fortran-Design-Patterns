module strategy_module

    implicit none
    private

    public :: add_type, sub_type, calculator_type

    type, abstract :: strategy_type
    contains
        procedure(strategy_type_calc), deferred :: calc
    end type strategy_type

    abstract interface
        integer function strategy_type_calc(self, a, b) result(c)
            import strategy_type
            class(strategy_type), intent(inout) :: self
            integer, intent(in) :: a, b
        end function strategy_type_calc
    end interface

    type, extends(strategy_type) :: add_type
    contains
        procedure :: calc => add_type_calc
    end type add_type

    type, extends(strategy_type) :: sub_type
    contains
        procedure :: calc => sub_type_calc
    end type sub_type

    type calculator_type
        class(strategy_type), pointer :: strategy
    contains
        procedure :: set_strategy => calculator_type_set_strategy
        procedure :: get_result => calculator_type_get_result
    end type calculator_type

contains

    integer function add_type_calc(self, a, b) result(c)
        class(add_type), intent(inout) :: self
        integer, intent(in) :: a, b
        c = a + b
    end function add_type_calc

    integer function sub_type_calc(self, a, b) result(c)
        class(sub_type), intent(inout) :: self
        integer, intent(in) :: a, b
        c = a - b
    end function sub_type_calc

    subroutine calculator_type_set_strategy(self, strategy)
        class(calculator_type), intent(inout) :: self
        class(strategy_type), intent(in), target :: strategy
        self%strategy => strategy
    end subroutine calculator_type_set_strategy

    integer function calculator_type_get_result(self, a, b) result(c)
        class(calculator_type), intent(inout) :: self
        integer, intent(in) :: a, b
        c = self%strategy%calc(a, b)
    end function calculator_type_get_result

end module strategy_module
