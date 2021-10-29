module memento_module

    implicit none
    private

    public :: caretaker_t, originator_t

    type originator_t
        character(:), allocatable :: state
    contains
        procedure :: create_memento => originator_t_create_memento
        procedure :: restore_memento => originator_t_restore_memento
        procedure :: set_state => originator_t_set_state
        procedure :: get_state => originator_t_get_state
    end type originator_t

    type memento_t
        character(:), allocatable :: state
    end type memento_t

    type caretaker_t
        type(memento_t), allocatable :: memento(:)
    contains
        procedure :: add_memento => caretaker_t_add_memento
        procedure :: get_memento => caretaker_t_get_memento
    end type caretaker_t

contains

    function originator_t_create_memento(self) result(memento)
        class(originator_t), intent(inout) :: self
        type(memento_t) :: memento
        memento%state = self%state
    end function originator_t_create_memento

    subroutine originator_t_restore_memento(self, memento)
        class(originator_t), intent(inout) :: self
        type(memento_t), intent(in) :: memento
        self%state = memento%state
    end subroutine originator_t_restore_memento

    subroutine originator_t_set_state(self, state)
        class(originator_t), intent(inout) :: self
        character(*), intent(in) :: state
        self%state = state
    end subroutine originator_t_set_state

    function originator_t_get_state(self) result(state)
        class(originator_t), intent(inout) :: self
        character(:), allocatable :: state
        state = self%state
    end function originator_t_get_state

    subroutine caretaker_t_add_memento(self, memento)
        class(caretaker_t), intent(inout) :: self
        type(memento_t), intent(in) :: memento
        self%memento = [self%memento, memento]
    end subroutine caretaker_t_add_memento

    function caretaker_t_get_memento(self, index) result(memento)
        class(caretaker_t), intent(inout) :: self
        integer, intent(in) :: index
        type(memento_t) :: memento
        memento = self%memento(index)
    end function caretaker_t_get_memento

end module memento_module
