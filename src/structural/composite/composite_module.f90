module composite_module

    implicit none
    private

    public :: file_type, folder_type

    type, abstract :: component_type
    contains
        procedure(component_type_search), deferred :: search
    end type component_type

    type, extends(component_type) :: file_type
        character(:), allocatable :: name
    contains
        procedure :: search => file_type_search
        procedure :: get_name => file_type_get_name
    end type file_type

    type node_t
        class(component_type), pointer :: node
    end type node_t

    type, extends(component_type) :: folder_type
        type(node_t), allocatable :: components(:)
        character(:), allocatable :: name
    contains
        procedure :: search => folder_type_search
    end type folder_type

    abstract interface
        subroutine component_type_search(self, keyward)
            import component_type
            class(component_type), intent(inout) :: self
            character(*), intent(in) :: keyward
        end subroutine component_type_search
    end interface

contains

    subroutine file_type_search(self, keyward)
        class(file_type), intent(inout) :: self
        character(*), intent(in) :: keyward
        print *, "Searching for keyword ", keyward, " in file ", self%name
    end subroutine file_type_search

    function file_type_get_name(self) result(name)
        class(file_type), intent(inout) :: self
        character(:), allocatable :: name
        name = self%name
    end function file_type_get_name

    ! - - - - - - - - - -

    subroutine folder_type_search(self, keyward)
        class(folder_type), intent(inout) :: self
        character(*), intent(in) :: keyward
        integer :: i
        print *, "Searching recursively for keyword ", keyward, " in folder ", self%name
        if (size(self%components) == 0) return
        do i = 1, size(self%components)
            call self%components(i)%node%search(keyward)
        end do
    end subroutine folder_type_search

end module composite_module
