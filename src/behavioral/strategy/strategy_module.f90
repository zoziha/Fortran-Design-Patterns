module strategy_module

    implicit none
    private

    public :: add_t, sub_t, calculator_t

    type, abstract :: strategy_t
    contains
        procedure(strategy_t_calc), deferred :: calc
    end type strategy_t

    abstract interface
        integer function strategy_t_calc(self, a, b) result(c)
            import strategy_t
            class(strategy_t), intent(inout) :: self
            integer, intent(in) :: a, b
        end function strategy_t_calc
    end interface

    type, extends(strategy_t) :: add_t
    contains
        procedure :: calc => add_t_calc
    end type add_t

    type, extends(strategy_t) :: sub_t
    contains
        procedure :: calc => sub_t_calc
    end type sub_t

    type calculator_t
        class(strategy_t), pointer :: strategy
    contains
        procedure :: set_strategy => calculator_t_set_strategy
        procedure :: get_result => calculator_t_get_result
    end type calculator_t

contains

    integer function add_t_calc(self, a, b) result(c)
        class(add_t), intent(inout) :: self
        integer, intent(in) :: a, b
        c = a + b
    end function add_t_calc

    integer function sub_t_calc(self, a, b) result(c)
        class(sub_t), intent(inout) :: self
        integer, intent(in) :: a, b
        c = a - b
    end function sub_t_calc

    subroutine calculator_t_set_strategy(self, strategy)
        class(calculator_t), intent(inout) :: self
        class(strategy_t), intent(in), target :: strategy
        self%strategy => strategy
    end subroutine calculator_t_set_strategy

    integer function calculator_t_get_result(self, a, b) result(c)
        class(calculator_t), intent(inout) :: self
        integer, intent(in) :: a, b
        c = self%strategy%calc(a, b)
    end function calculator_t_get_result

end module strategy_module
