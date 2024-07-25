module state_module

    implicit none
    private

    public :: person_type

    type :: hungry_state_type
        logical :: state
    contains
        procedure :: hungry => hungry_state_t_hungry
        procedure :: no_hungry => hungry_state_t_no_hungry
    end type hungry_state_type

    type :: person_type
        type(hungry_state_type) :: hungry_state
    contains
        procedure :: eat => person_t_eat
        procedure :: work => person_t_work
    end type

contains

    subroutine person_t_eat(self)
        class(person_type), intent(inout) :: self
        if (self%hungry_state%state) then
            print *, "Eatting.."
            !!// 改变状态
            call self%hungry_state%no_hungry
        else
            print *, "Already baole!!"
        end if
    end subroutine person_t_eat

    subroutine person_t_work(self)
        class(person_type), intent(inout) :: self
        if (self%hungry_state%state) then
            print *, "I am hungry, no work!!"
        else
            print *, "Ok, let us do work.."
            call self%hungry_state%hungry
        end if
    end subroutine person_t_work

    subroutine hungry_state_t_hungry(self)
        class(hungry_state_type), intent(inout) :: self
        self%state = .true.
    end subroutine hungry_state_t_hungry

    subroutine hungry_state_t_no_hungry(self)
        class(hungry_state_type), intent(inout) :: self
        self%state = .false.
    end subroutine hungry_state_t_no_hungry

end module state_module
