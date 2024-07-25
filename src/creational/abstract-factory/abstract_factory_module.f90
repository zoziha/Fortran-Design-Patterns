module abstract_factory_module

    use, intrinsic :: iso_fortran_env, only: int8
    implicit none
    private

    public :: isports_factory_type, erke_type, lining_type, get_sports_factory, erke_shoe_type, &
              erke_shirt_type, lining_shoe_type, lining_shirt_type, ishoe_type, ishirt_type

    !> Abstract classes
    type, abstract :: isports_factory_type
    contains
        procedure(isports_factory_type_make_shoe), deferred :: make_shoe
        procedure(isports_factory_type_make_shirt), deferred :: make_shirt
    end type isports_factory_type

    type, abstract :: ishoe_type
    contains
        procedure(ishoe_type_set_logo), deferred :: set_logo
        procedure(ishoe_type_set_size), deferred :: set_size
        procedure(ishoe_type_get_logo), deferred :: get_logo
        procedure(ishoe_type_get_size), deferred :: get_size
    end type ishoe_type

    type, abstract :: ishirt_type
    contains
        procedure(ishirt_type_set_logo), deferred :: set_logo
        procedure(ishirt_type_set_size), deferred :: set_size
        procedure(ishirt_type_get_logo), deferred :: get_logo
        procedure(ishirt_type_get_size), deferred :: get_size
    end type ishirt_type

    abstract interface

        function isports_factory_type_make_shoe(self) result(shoe)
            import isports_factory_type, ishoe_type
            class(isports_factory_type), intent(inout) :: self
            class(ishoe_type), allocatable :: shoe
        end function isports_factory_type_make_shoe
        function isports_factory_type_make_shirt(self) result(shirt)
            import isports_factory_type, ishirt_type
            class(isports_factory_type), intent(inout) :: self
            class(ishirt_type), allocatable :: shirt
        end function isports_factory_type_make_shirt

        subroutine ishoe_type_set_logo(self, logo)
            import ishoe_type
            class(ishoe_type), intent(inout) :: self
            character(*), intent(in) :: logo
        end subroutine ishoe_type_set_logo
        subroutine ishoe_type_set_size(self, size)
            import ishoe_type, int8
            class(ishoe_type), intent(inout) :: self
            integer(int8), intent(in) :: size
        end subroutine ishoe_type_set_size
        function ishoe_type_get_logo(self) result(logo)
            import ishoe_type
            class(ishoe_type), intent(inout) :: self
            character(:), allocatable :: logo
        end function ishoe_type_get_logo
        function ishoe_type_get_size(self) result(size)
            import ishoe_type, int8
            class(ishoe_type), intent(inout) :: self
            integer(int8) :: size
        end function ishoe_type_get_size

        subroutine ishirt_type_set_logo(self, logo)
            import ishirt_type
            class(ishirt_type), intent(inout) :: self
            character(*), intent(in) :: logo
        end subroutine ishirt_type_set_logo
        subroutine ishirt_type_set_size(self, size)
            import ishirt_type, int8
            class(ishirt_type), intent(inout) :: self
            integer(int8), intent(in) :: size
        end subroutine ishirt_type_set_size
        function ishirt_type_get_logo(self) result(logo)
            import ishirt_type
            class(ishirt_type), intent(inout) :: self
            character(:), allocatable :: logo
        end function ishirt_type_get_logo
        function ishirt_type_get_size(self) result(size)
            import ishirt_type, int8
            class(ishirt_type), intent(inout) :: self
            integer(int8) :: size
        end function ishirt_type_get_size

    end interface

    !> Specific objects

    type, extends(isports_factory_type) :: erke_type
    contains
        procedure :: make_shoe => erke_type_make_shoe
        procedure :: make_shirt => erke_type_make_shirt
    end type erke_type

    type, extends(isports_factory_type) :: lining_type
    contains
        procedure :: make_shoe => lining_type_make_shoe
        procedure :: make_shirt => lining_type_make_shirt
    end type lining_type

    type, extends(ishoe_type) :: shoe_type
        character(:), allocatable :: logo
        integer(int8) :: size
    contains
        procedure :: set_logo => shoe_type_set_logo
        procedure :: set_size => shoe_type_set_size
        procedure :: get_logo => shoe_type_get_logo
        procedure :: get_size => shoe_type_get_size
    end type shoe_type

    type, extends(ishirt_type) :: shirt_type
        character(:), allocatable :: logo
        integer(int8) :: size
    contains
        procedure :: set_logo => shirt_type_set_logo
        procedure :: set_size => shirt_type_set_size
        procedure :: get_logo => shirt_type_get_logo
        procedure :: get_size => shirt_type_get_size
    end type shirt_type

    type, extends(shoe_type) :: erke_shoe_type
    end type erke_shoe_type

    type, extends(shoe_type) :: lining_shoe_type
    end type lining_shoe_type

    type, extends(shirt_type) :: erke_shirt_type
    end type erke_shirt_type

    type, extends(shirt_type) :: lining_shirt_type
    end type lining_shirt_type

contains

    function get_sports_factory(brand) result(isports_factory)
        character(*), intent(in) :: brand
        class(isports_factory_type), allocatable :: isports_factory

        select case (brand)
        case ("erke")
            isports_factory = erke_type()
        case ("lining")
            isports_factory = lining_type()
        case default
            error stop "*<ERROR>* Brand not supported."
        end select

    end function get_sports_factory

    function erke_type_make_shoe(self) result(shoe)
        class(erke_type), intent(inout) :: self
        class(ishoe_type), allocatable :: shoe

        shoe = erke_shoe_type(logo="erke", size=15_int8)

    end function erke_type_make_shoe

    function erke_type_make_shirt(self) result(shirt)
        class(erke_type), intent(inout) :: self
        class(ishirt_type), allocatable :: shirt

        shirt = erke_shirt_type(logo="erke", size=84_int8)

    end function erke_type_make_shirt

    function lining_type_make_shoe(self) result(shoe)
        class(lining_type), intent(inout) :: self
        class(ishoe_type), allocatable :: shoe

        shoe = lining_shoe_type(logo="lining", size=14_int8)

    end function lining_type_make_shoe

    function lining_type_make_shirt(self) result(shirt)
        class(lining_type), intent(inout) :: self
        class(ishirt_type), allocatable :: shirt

        shirt = lining_shirt_type(logo="lining", size=85_int8)

    end function lining_type_make_shirt

    subroutine shoe_type_set_logo(self, logo)
        class(shoe_type), intent(inout) :: self
        character(*), intent(in) :: logo

        self%logo = logo

    end subroutine shoe_type_set_logo

    subroutine shoe_type_set_size(self, size)
        class(shoe_type), intent(inout) :: self
        integer(int8), intent(in) :: size

        self%size = size

    end subroutine shoe_type_set_size

    function shoe_type_get_logo(self) result(logo)
        class(shoe_type), intent(inout) :: self
        character(:), allocatable :: logo

        logo = self%logo

    end function shoe_type_get_logo

    function shoe_type_get_size(self) result(size)
        class(shoe_type), intent(inout) :: self
        integer(int8) :: size

        size = self%size

    end function shoe_type_get_size

    subroutine shirt_type_set_logo(self, logo)
        class(shirt_type), intent(inout) :: self
        character(*), intent(in) :: logo

        self%logo = logo

    end subroutine shirt_type_set_logo

    subroutine shirt_type_set_size(self, size)
        class(shirt_type), intent(inout) :: self
        integer(int8), intent(in) :: size

        self%size = size

    end subroutine shirt_type_set_size

    function shirt_type_get_logo(self) result(logo)
        class(shirt_type), intent(inout) :: self
        character(:), allocatable :: logo

        logo = self%logo

    end function shirt_type_get_logo

    function shirt_type_get_size(self) result(size)
        class(shirt_type), intent(inout) :: self
        integer(int8) :: size

        size = self%size

    end function shirt_type_get_size

end module abstract_factory_module
