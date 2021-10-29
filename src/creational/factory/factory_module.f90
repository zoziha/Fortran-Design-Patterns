module factory_module

    use, intrinsic :: iso_fortran_env, only: int8
    implicit none
    private

    public :: igun_t, ak47_t, musket_t, get_gun

    type, abstract :: igun_t
    contains
        procedure(igun_t_set_name), deferred :: set_name
        procedure(igun_t_set_power), deferred :: set_power
        procedure(igun_t_get_name), deferred :: get_name
        procedure(igun_t_get_power), deferred :: get_power
    end type igun_t

    abstract interface

        subroutine igun_t_set_name(self, name)
            import igun_t
            class(igun_t), intent(inout) :: self
            character(*), intent(in) :: name
        end subroutine igun_t_set_name

        subroutine igun_t_set_power(self, power)
            import igun_t, int8
            class(igun_t), intent(inout) :: self
            integer(int8), intent(in) :: power
        end subroutine igun_t_set_power

        function igun_t_get_name(self) result(name)
            import igun_t
            class(igun_t), intent(inout) :: self
            character(:), allocatable :: name
        end function igun_t_get_name

        function igun_t_get_power(self) result(power)
            import igun_t, int8
            class(igun_t), intent(inout) :: self
            integer(int8) :: power
        end function igun_t_get_power

    end interface

    type, extends(igun_t) :: gun_t
        character(:), allocatable :: name
        integer(int8) :: power
    contains
        procedure :: set_name => gun_t_set_name
        procedure :: get_name => gun_t_get_name
        procedure :: set_power => gun_t_set_power
        procedure :: get_power => gun_t_get_power
    end type gun_t

    type, extends(gun_t) :: ak47_t
    end type ak47_t

    type, extends(gun_t) :: musket_t
    end type musket_t

contains

    subroutine gun_t_set_name(self, name)
        class(gun_t), intent(inout) :: self
        character(*), intent(in) :: name
        self%name = name
    end subroutine gun_t_set_name

    subroutine gun_t_set_power(self, power)
        class(gun_t), intent(inout) :: self
        integer(int8), intent(in) :: power
        self%power = power
    end subroutine gun_t_set_power

    function gun_t_get_name(self) result(name)
        class(gun_t), intent(inout) :: self
        character(:), allocatable :: name
        name = self%name
    end function gun_t_get_name

    function gun_t_get_power(self) result(power)
        class(gun_t), intent(inout) :: self
        integer(int8) :: power
        power = self%power
    end function gun_t_get_power

    function get_gun(gun_type) result(igun)
        character(*), intent(in) :: gun_type
        class(igun_t), allocatable :: igun

        select case (gun_type)
        case ("ak47")
            igun = ak47_t(name="ak47 gun", power=4)
        case ("musket")
            igun = musket_t(name="musket gun", power=1)
        case default
            error stop "*ERROR* `gnu_type` not supported"
        end select

    end function get_gun

end module factory_module
