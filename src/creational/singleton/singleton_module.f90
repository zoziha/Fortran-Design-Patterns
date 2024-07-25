module singleton_module

    implicit none
    private

    public :: single, get_instance, dispose_instance

    logical :: lock = .false.

    type single_type
        private
        integer :: value
    end type single_type

    type(single_type) :: single

contains

    function get_instance(value) result(single)
        integer, intent(in) :: value
        type(single_type) :: single
        if (lock) then
            print *, "Single instance already created."
            return
        else
            print *, "Creating single instance now."
            single%value = value
            lock = .true.
        end if
    end function get_instance

    subroutine dispose_instance(single)
        type(single_type), intent(inout) :: single
        print *, "Disposing single instance now."
        single%value = 0
        lock = .false.
    end subroutine dispose_instance

end module singleton_module
