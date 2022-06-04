!>
!> 策略模式更倾向于方法策略，而不是类策略的时候，可以使用函数方法代替类声明。
module strategy_extends_m

    implicit none
    private

    public :: calculator_t

    type calculator_t
        procedure(fcn), nopass, pointer :: strategy
    contains
        procedure, pass :: set_strategy => calculator_t_set_strategy
        procedure, pass :: calc => calculator_t_calc
    end type calculator_t

    abstract interface
        integer function fcn(a, b) result(c)
            integer, intent(in) :: a, b
        end function fcn
    end interface

contains

    subroutine calculator_t_set_strategy(self, strategy)
        class(calculator_t), intent(inout) :: self
        procedure(fcn) :: strategy
        self%strategy => strategy
    end subroutine calculator_t_set_strategy

    integer function calculator_t_calc(self, a, b) result(c)
        class(calculator_t), intent(in) :: self
        integer, intent(in) :: a, b
        c = self%strategy(a, b)
    end function calculator_t_calc

end module strategy_extends_m

program main

    use strategy_extends_m, only: calculator_t
    implicit none
    type(calculator_t) :: calculator

    call calculator%set_strategy(add)
    print *, calculator%calc(1, 1)

    call calculator%set_strategy(sub)
    print *, calculator%calc(1, 1)

contains

    integer function add(a, b) result(c)
        integer, intent(in) :: a, b
        c = a + b
    end function add

    integer function sub(a, b) result(c)
        integer, intent(in) :: a, b
        c = a - b
    end function sub

end program main
