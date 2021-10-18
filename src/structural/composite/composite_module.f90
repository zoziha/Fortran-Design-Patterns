module composite_module

    implicit none
    private

    public :: file_t, folder_t
    
    type, abstract :: component_t
    contains
    procedure(component_t_search), deferred :: search
    end type component_t
    
    type, extends(component_t) :: file_t
    character(:), allocatable :: name
    contains
    procedure :: search => file_t_search
    procedure :: get_name => file_t_get_name
    end type file_t
    
    type node_t
    class(component_t), pointer :: node
    end type node_t
    
    type, extends(component_t) :: folder_t
    type(node_t), allocatable :: components(:)
    character(:), allocatable :: name
    contains
    procedure :: search => folder_t_search
    end type folder_t
    
    abstract interface
        subroutine component_t_search(self, keyward)
        import component_t
        class(component_t), intent(inout) :: self
        character(*), intent(in) :: keyward
        end subroutine component_t_search
    end interface
    
contains

    subroutine file_t_search(self, keyward)
    class(file_t), intent(inout) :: self
    character(*), intent(in) :: keyward
        print *, "Searching for keyword ", keyward, " in file ", self%name
    end subroutine file_t_search
    
    function file_t_get_name(self) result(name)
    class(file_t), intent(inout) :: self
    character(:), allocatable :: name
        name = self%name
    end function file_t_get_name
    
    ! - - - - - - - - - -
    
    subroutine folder_t_search(self, keyward)
    class(folder_t), intent(inout) :: self
    character(*), intent(in) :: keyward
    integer :: i
        print *, "Searching recursively for keyword ", keyward, " in folder ", self%name
        if (size(self%components) == 0) return
        do i = 1, size(self%components)
        call self%components(i)%node%search(keyward)
        end do
    end subroutine folder_t_search
    
end module composite_module