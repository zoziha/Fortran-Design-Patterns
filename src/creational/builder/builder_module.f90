module builder_module

    use, intrinsic :: iso_fortran_env, only: int8
    implicit none
    private

    public :: ibuilder_type, director_type, house_type, get_builder

    type, abstract :: ibuilder_type
    contains
        procedure(ibuilder_type_set_window_type), deferred :: set_window_type
        procedure(ibuilder_type_set_door_type), deferred :: set_door_type
        procedure(ibuilder_type_set_num_floor), deferred :: set_num_floor
        procedure(ibuilder_type_get_house), deferred :: get_house
    end type ibuilder_type

    type, extends(ibuilder_type) :: normal_builder_type
        character(:), allocatable :: window_type
        character(:), allocatable :: door_type
        integer(int8) :: floor
    contains
        procedure :: set_window_type => normal_builder_type_set_window_type
        procedure :: set_door_type => normal_builder_type_set_door_type
        procedure :: set_num_floor => normal_builder_type_set_num_floor
        procedure :: get_house => normal_builder_type_get_house
    end type normal_builder_type

    type, extends(ibuilder_type) :: igloo_builder_type
        character(:), allocatable :: window_type
        character(:), allocatable :: door_type
        integer(int8) :: floor
    contains
        procedure :: set_window_type => igloo_builder_type_set_window_type
        procedure :: set_door_type => igloo_builder_type_set_door_type
        procedure :: set_num_floor => igloo_builder_type_set_num_floor
        procedure :: get_house => igloo_builder_type_get_house
    end type igloo_builder_type

    type house_type
        character(:), allocatable :: window_type
        character(:), allocatable :: door_type
        integer(int8) :: floor
    end type house_type

    type director_type
        class(ibuilder_type), pointer :: builder
    contains
        procedure :: set_builder => director_type_set_builder
        procedure :: build_house => director_type_build_house
    end type director_type

    abstract interface

        subroutine ibuilder_type_set_window_type(self)
            import ibuilder_type
            class(ibuilder_type), intent(inout) :: self
        end subroutine ibuilder_type_set_window_type

        subroutine ibuilder_type_set_door_type(self)
            import ibuilder_type
            class(ibuilder_type), intent(inout) :: self
        end subroutine ibuilder_type_set_door_type

        subroutine ibuilder_type_set_num_floor(self)
            import ibuilder_type
            class(ibuilder_type), intent(inout) :: self
        end subroutine ibuilder_type_set_num_floor

        function ibuilder_type_get_house(self) result(house)
            import ibuilder_type, house_type
            class(ibuilder_type), intent(inout) :: self
            type(house_type) :: house
        end function ibuilder_type_get_house

    end interface

contains

    function get_builder(builder_type) result(ibuilder)
        character(*), intent(in) :: builder_type
        class(ibuilder_type), allocatable :: ibuilder
        select case (builder_type)
        case ("normal")
            allocate (normal_builder_type :: ibuilder)
        case ("igloo")
            allocate (igloo_builder_type :: ibuilder)
        end select
    end function get_builder

    ! - - - - - - - - - -

    subroutine normal_builder_type_set_window_type(self)
        class(normal_builder_type), intent(inout) :: self
        self%window_type = "Wooden Window"
    end subroutine normal_builder_type_set_window_type

    subroutine normal_builder_type_set_door_type(self)
        class(normal_builder_type), intent(inout) :: self
        self%door_type = "Wooden Door"
    end subroutine normal_builder_type_set_door_type

    subroutine normal_builder_type_set_num_floor(self)
        class(normal_builder_type), intent(inout) :: self
        self%floor = 2_int8
    end subroutine normal_builder_type_set_num_floor

    function normal_builder_type_get_house(self) result(house)
        class(normal_builder_type), intent(inout) :: self
        type(house_type) :: house
        ! TODO: A GFortran Bug Here.
        ! house = house_t(door_type=self%door_type, &
        !                 window_type=self%window_type, &
        !                 floor=self%floor)
        house%door_type = self%door_type
        house%window_type = self%window_type
        house%floor = self%floor
    end function normal_builder_type_get_house

    ! - - - - - - - - - -

    subroutine igloo_builder_type_set_window_type(self)
        class(igloo_builder_type), intent(inout) :: self
        self%window_type = "Snow Window"
    end subroutine igloo_builder_type_set_window_type

    subroutine igloo_builder_type_set_door_type(self)
        class(igloo_builder_type), intent(inout) :: self
        self%door_type = "Snow Door"
    end subroutine igloo_builder_type_set_door_type

    subroutine igloo_builder_type_set_num_floor(self)
        class(igloo_builder_type), intent(inout) :: self
        self%floor = 1_int8
    end subroutine igloo_builder_type_set_num_floor

    function igloo_builder_type_get_house(self) result(house)
        class(igloo_builder_type), intent(inout) :: self
        type(house_type) :: house
        ! house = house_t(door_type=self%door_type, &
        !                 window_type=self%window_type, &
        !                 floor=self%floor)
        house%door_type = self%door_type
        house%window_type = self%window_type
        house%floor = self%floor
    end function igloo_builder_type_get_house

    ! - - - - - - - - - -

    subroutine director_type_set_builder(self, b)
        class(director_type), intent(inout) :: self
        class(ibuilder_type), intent(inout), target :: b
        self%builder => b
    end subroutine director_type_set_builder

    function director_type_build_house(self) result(house)
        class(director_type), intent(inout) :: self
        type(house_type) :: house
        call self%builder%set_door_type()
        call self%builder%set_window_type()
        call self%builder%set_num_floor()
        house = self%builder%get_house()
    end function director_type_build_house

end module builder_module
