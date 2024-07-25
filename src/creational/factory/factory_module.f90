module factory_module

    use, intrinsic :: iso_fortran_env, only: int8
    implicit none
    private

    public :: igun_type, ak47_type, musket_type, get_gun

    type, abstract :: igun_type
    contains
        procedure(igun_type_set_name), deferred :: set_name
        procedure(igun_type_set_power), deferred :: set_power
        procedure(igun_type_get_name), deferred :: get_name
        procedure(igun_type_get_power), deferred :: get_power
    end type igun_type

    abstract interface

        subroutine igun_type_set_name(self, name)
            import igun_type
            class(igun_type), intent(inout) :: self
            character(*), intent(in) :: name
        end subroutine igun_type_set_name

        subroutine igun_type_set_power(self, power)
            import igun_type, int8
            class(igun_type), intent(inout) :: self
            integer(int8), intent(in) :: power
        end subroutine igun_type_set_power

        function igun_type_get_name(self) result(name)
            import igun_type
            class(igun_type), intent(inout) :: self
            character(:), allocatable :: name
        end function igun_type_get_name

        function igun_type_get_power(self) result(power)
            import igun_type, int8
            class(igun_type), intent(inout) :: self
            integer(int8) :: power
        end function igun_type_get_power

    end interface

    type, extends(igun_type) :: gun_type
        character(:), allocatable :: name
        integer(int8) :: power
    contains
        procedure :: set_name => gun_type_set_name
        procedure :: get_name => gun_type_get_name
        procedure :: set_power => gun_type_set_power
        procedure :: get_power => gun_type_get_power
    end type gun_type

    type, extends(gun_type) :: ak47_type
    end type ak47_type

    type, extends(gun_type) :: musket_type
    end type musket_type

contains

    subroutine gun_type_set_name(self, name)
        class(gun_type), intent(inout) :: self
        character(*), intent(in) :: name
        self%name = name
    end subroutine gun_type_set_name

    subroutine gun_type_set_power(self, power)
        class(gun_type), intent(inout) :: self
        integer(int8), intent(in) :: power
        self%power = power
    end subroutine gun_type_set_power

    function gun_type_get_name(self) result(name)
        class(gun_type), intent(inout) :: self
        character(:), allocatable :: name
        name = self%name
    end function gun_type_get_name

    function gun_type_get_power(self) result(power)
        class(gun_type), intent(inout) :: self
        integer(int8) :: power
        power = self%power
    end function gun_type_get_power

    function get_gun(gun_type) result(igun)
        character(*), intent(in) :: gun_type
        class(igun_type), allocatable :: igun

        select case (gun_type)
        case ("ak47")
            igun = ak47_type(name="ak47 gun", power=4)
        case ("musket")
            igun = musket_type(name="musket gun", power=1)
        case default
            error stop "*ERROR* `gnu_type` not supported"
        end select

    end function get_gun

end module factory_module
