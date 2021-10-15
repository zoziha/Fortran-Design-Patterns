module abstract_factory_module

    use, intrinsic :: iso_fortran_env, only: int8
    implicit none
    private
    
    public :: isports_factory_t, erke_t, lining_t, get_sports_factory, erke_shoe_t, erke_shirt_t, &
        lining_shoe_t, lining_shirt_t, ishoe_t, ishirt_t
    
    !> Abstract classes
    type, abstract :: isports_factory_t
    contains
        procedure(isports_factory_t_make_shoe) , deferred :: make_shoe
        procedure(isports_factory_t_make_shirt), deferred :: make_shirt
    end type isports_factory_t
    
    type, abstract :: ishoe_t
    contains
        procedure(ishoe_t_set_logo), deferred :: set_logo
        procedure(ishoe_t_set_size), deferred :: set_size
        procedure(ishoe_t_get_logo), deferred :: get_logo
        procedure(ishoe_t_get_size), deferred :: get_size
    end type ishoe_t
    
    type, abstract :: ishirt_t
    contains
        procedure(ishirt_t_set_logo), deferred :: set_logo
        procedure(ishirt_t_set_size), deferred :: set_size
        procedure(ishirt_t_get_logo), deferred :: get_logo
        procedure(ishirt_t_get_size), deferred :: get_size
    end type ishirt_t
    
    abstract interface
    
        function isports_factory_t_make_shoe(self) result(shoe)
            import isports_factory_t, ishoe_t
            class(isports_factory_t), intent(inout) :: self
            class(ishoe_t), allocatable :: shoe
        end function isports_factory_t_make_shoe
        function isports_factory_t_make_shirt(self) result(shirt)
            import isports_factory_t, ishirt_t
            class(isports_factory_t), intent(inout) :: self
            class(ishirt_t), allocatable :: shirt
        end function isports_factory_t_make_shirt
        
        subroutine ishoe_t_set_logo(self, logo)
            import ishoe_t
            class(ishoe_t), intent(inout) :: self
            character(*), intent(in) :: logo
        end subroutine ishoe_t_set_logo
        subroutine ishoe_t_set_size(self, size)
            import ishoe_t, int8
            class(ishoe_t), intent(inout) :: self
            integer(int8), intent(in) :: size
        end subroutine ishoe_t_set_size
        function ishoe_t_get_logo(self) result(logo)
            import ishoe_t
            class(ishoe_t), intent(inout) :: self
            character(:), allocatable :: logo
        end function ishoe_t_get_logo
        function ishoe_t_get_size(self) result(size)
            import ishoe_t, int8
            class(ishoe_t), intent(inout) :: self
            integer(int8) :: size
        end function ishoe_t_get_size
        
        subroutine ishirt_t_set_logo(self, logo)
            import ishirt_t
            class(ishirt_t), intent(inout) :: self
            character(*), intent(in) :: logo
        end subroutine ishirt_t_set_logo
        subroutine ishirt_t_set_size(self, size)
            import ishirt_t, int8
            class(ishirt_t), intent(inout) :: self
            integer(int8), intent(in) :: size
        end subroutine ishirt_t_set_size
        function ishirt_t_get_logo(self) result(logo)
            import ishirt_t
            class(ishirt_t), intent(inout) :: self
            character(:), allocatable :: logo
        end function ishirt_t_get_logo
        function ishirt_t_get_size(self) result(size)
            import ishirt_t, int8
            class(ishirt_t), intent(inout) :: self
            integer(int8) :: size
        end function ishirt_t_get_size
         
    end interface
    
    !> Specific objects
    
    type, extends(isports_factory_t) :: erke_t
    contains
        procedure :: make_shoe  => erke_t_make_shoe
        procedure :: make_shirt => erke_t_make_shirt
    end type erke_t
    
    type, extends(isports_factory_t) :: lining_t
    contains
        procedure :: make_shoe  => lining_t_make_shoe
        procedure :: make_shirt => lining_t_make_shirt
    end type lining_t
    
    type, extends(ishoe_t) :: shoe_t
        character(:), allocatable :: logo
        integer(int8) :: size
    contains
        procedure :: set_logo => shoe_t_set_logo
        procedure :: set_size => shoe_t_set_size
        procedure :: get_logo => shoe_t_get_logo
        procedure :: get_size => shoe_t_get_size
    end type shoe_t
    
    type, extends(ishirt_t) :: shirt_t
        character(:), allocatable :: logo
        integer(int8) :: size
    contains
        procedure :: set_logo => shirt_t_set_logo
        procedure :: set_size => shirt_t_set_size
        procedure :: get_logo => shirt_t_get_logo
        procedure :: get_size => shirt_t_get_size
    end type shirt_t
    
    type, extends(shoe_t) :: erke_shoe_t
    end type erke_shoe_t
    
    type, extends(shoe_t) :: lining_shoe_t
    end type lining_shoe_t
    
    type, extends(shirt_t) :: erke_shirt_t
    end type erke_shirt_t
    
    type, extends(shirt_t) :: lining_shirt_t
    end type lining_shirt_t
    
contains

    function get_sports_factory(brand) result(isports_factory)
        character(*), intent(in) :: brand
        class(isports_factory_t), allocatable :: isports_factory
        
        select case (brand)
        case ("erke")
            isports_factory = erke_t()
        case ("lining")
            isports_factory = lining_t()
        case default
            error stop "*<ERROR>* Brand not supported."
        end select
        
    end function get_sports_factory

    function erke_t_make_shoe(self) result(shoe)
        class(erke_t), intent(inout) :: self
        class(ishoe_t), allocatable  :: shoe
        
        shoe = erke_shoe_t(logo="erke", size=14_int8)
        
    end function erke_t_make_shoe
    
    function erke_t_make_shirt(self) result(shirt)
        class(erke_t), intent(inout) :: self
        class(ishirt_t), allocatable :: shirt
        
        shirt = erke_shirt_t(logo="erke", size=14_int8)
        
    end function erke_t_make_shirt
    
    function lining_t_make_shoe(self) result(shoe)
        class(lining_t), intent(inout) :: self
        class(ishoe_t), allocatable :: shoe
        
        shoe = lining_shoe_t(logo="lining", size=14_int8)
        
    end function lining_t_make_shoe
    
    function lining_t_make_shirt(self) result(shirt)
        class(lining_t), intent(inout) :: self
        class(ishirt_t), allocatable :: shirt
        
        shirt = lining_shirt_t(logo="lining", size=14_int8)
        
    end function lining_t_make_shirt
    
    subroutine shoe_t_set_logo(self, logo)
        class(shoe_t), intent(inout) :: self
        character(*), intent(in) :: logo
        
        self%logo = logo
        
    end subroutine shoe_t_set_logo
    
    subroutine shoe_t_set_size(self, size)
        class(shoe_t), intent(inout) :: self
        integer(int8), intent(in) :: size
        
        self%size = size
        
    end subroutine shoe_t_set_size
    
    function shoe_t_get_logo(self) result(logo)
        class(shoe_t), intent(inout) :: self
        character(:), allocatable :: logo
        
        logo = self%logo
        
    end function shoe_t_get_logo
    
    function shoe_t_get_size(self) result(size)
        class(shoe_t), intent(inout) :: self
        integer(int8) :: size
        
        size = self%size
        
    end function shoe_t_get_size
    
    subroutine shirt_t_set_logo(self, logo)
        class(shirt_t), intent(inout) :: self
        character(*), intent(in) :: logo
        
        self%logo = logo
        
    end subroutine shirt_t_set_logo
    
    subroutine shirt_t_set_size(self, size)
        class(shirt_t), intent(inout) :: self
        integer(int8), intent(in) :: size
        
        self%size = size
        
    end subroutine shirt_t_set_size
    
    function shirt_t_get_logo(self) result(logo)
        class(shirt_t), intent(inout) :: self
        character(:), allocatable :: logo
        
        logo = self%logo
        
    end function shirt_t_get_logo
    
    function shirt_t_get_size(self) result(size)
        class(shirt_t), intent(inout) :: self
        integer(int8) :: size
        
        size = self%size
        
    end function shirt_t_get_size

end module abstract_factory_module