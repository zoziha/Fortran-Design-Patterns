module builder_module

    use, intrinsic :: iso_fortran_env, only: int8
    implicit none
    private

    public :: ibuilder_t, director_t, house_t, get_builder

    type, abstract :: ibuilder_t
    contains
        procedure(ibuilder_t_set_window_type), deferred :: set_window_type
        procedure(ibuilder_t_set_door_type), deferred :: set_door_type
        procedure(ibuilder_t_set_num_floor), deferred :: set_num_floor
        procedure(ibuilder_t_get_house), deferred :: get_house
    end type ibuilder_t

    type, extends(ibuilder_t) :: normal_builder_t
        character(:), allocatable :: window_type
        character(:), allocatable :: door_type
        integer(int8) :: floor
    contains
        procedure :: set_window_type => normal_builder_t_set_window_type
        procedure :: set_door_type => normal_builder_t_set_door_type
        procedure :: set_num_floor => normal_builder_t_set_num_floor
        procedure :: get_house => normal_builder_t_get_house
    end type normal_builder_t

    type, extends(ibuilder_t) :: igloo_builder_t
        character(:), allocatable :: window_type
        character(:), allocatable :: door_type
        integer(int8) :: floor
    contains
        procedure :: set_window_type => igloo_builder_t_set_window_type
        procedure :: set_door_type => igloo_builder_t_set_door_type
        procedure :: set_num_floor => igloo_builder_t_set_num_floor
        procedure :: get_house => igloo_builder_t_get_house
    end type igloo_builder_t

    type house_t
        character(:), allocatable :: window_type
        character(:), allocatable :: door_type
        integer(int8) :: floor
    end type house_t

    type director_t
        class(ibuilder_t), pointer :: builder
    contains
        procedure :: set_builder => director_t_set_builder
        procedure :: build_house => director_t_build_house
    end type director_t

    abstract interface

        subroutine ibuilder_t_set_window_type(self)
            import ibuilder_t
            class(ibuilder_t), intent(inout) :: self
        end subroutine ibuilder_t_set_window_type

        subroutine ibuilder_t_set_door_type(self)
            import ibuilder_t
            class(ibuilder_t), intent(inout) :: self
        end subroutine ibuilder_t_set_door_type

        subroutine ibuilder_t_set_num_floor(self)
            import ibuilder_t
            class(ibuilder_t), intent(inout) :: self
        end subroutine ibuilder_t_set_num_floor

        function ibuilder_t_get_house(self) result(house)
            import ibuilder_t, house_t
            class(ibuilder_t), intent(inout) :: self
            type(house_t) :: house
        end function ibuilder_t_get_house

    end interface

contains

    function get_builder(builder_type) result(ibuilder)
        character(*), intent(in) :: builder_type
        class(ibuilder_t), allocatable :: ibuilder
        select case (builder_type)
        case ("normal")
            allocate (normal_builder_t :: ibuilder)
        case ("igloo")
            allocate (igloo_builder_t :: ibuilder)
        end select
    end function get_builder

    ! - - - - - - - - - -

    subroutine normal_builder_t_set_window_type(self)
        class(normal_builder_t), intent(inout) :: self
        self%window_type = "Wooden Window"
    end subroutine normal_builder_t_set_window_type

    subroutine normal_builder_t_set_door_type(self)
        class(normal_builder_t), intent(inout) :: self
        self%door_type = "Wooden Door"
    end subroutine normal_builder_t_set_door_type

    subroutine normal_builder_t_set_num_floor(self)
        class(normal_builder_t), intent(inout) :: self
        self%floor = 2_int8
    end subroutine normal_builder_t_set_num_floor

    function normal_builder_t_get_house(self) result(house)
        class(normal_builder_t), intent(inout) :: self
        type(house_t) :: house
        ! TODO: A GFortran Bug Here.
        ! house = house_t(door_type=self%door_type, &
        !                 window_type=self%window_type, &
        !                 floor=self%floor)
        house%door_type = self%door_type
        house%window_type = self%window_type
        house%floor = self%floor
    end function normal_builder_t_get_house

    ! - - - - - - - - - -

    subroutine igloo_builder_t_set_window_type(self)
        class(igloo_builder_t), intent(inout) :: self
        self%window_type = "Snow Window"
    end subroutine igloo_builder_t_set_window_type

    subroutine igloo_builder_t_set_door_type(self)
        class(igloo_builder_t), intent(inout) :: self
        self%door_type = "Snow Door"
    end subroutine igloo_builder_t_set_door_type

    subroutine igloo_builder_t_set_num_floor(self)
        class(igloo_builder_t), intent(inout) :: self
        self%floor = 1_int8
    end subroutine igloo_builder_t_set_num_floor

    function igloo_builder_t_get_house(self) result(house)
        class(igloo_builder_t), intent(inout) :: self
        type(house_t) :: house
        ! house = house_t(door_type=self%door_type, &
        !                 window_type=self%window_type, &
        !                 floor=self%floor)
        house%door_type = self%door_type
        house%window_type = self%window_type
        house%floor = self%floor
    end function igloo_builder_t_get_house

    ! - - - - - - - - - -

    subroutine director_t_set_builder(self, b)
        class(director_t), intent(inout) :: self
        class(ibuilder_t), intent(inout), target :: b
        self%builder => b
    end subroutine director_t_set_builder

    function director_t_build_house(self) result(house)
        class(director_t), intent(inout) :: self
        type(house_t) :: house
        call self%builder%set_door_type()
        call self%builder%set_window_type()
        call self%builder%set_num_floor()
        house = self%builder%get_house()
    end function director_t_build_house

end module builder_module
