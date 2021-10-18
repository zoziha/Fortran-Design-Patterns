module prototype_module

    implicit none
    private
    
    public :: file_t, folder_t, inode_t
    
    type, abstract :: inode_t
    contains
    procedure(inode_t_print), deferred :: print
    procedure(inode_t_clone), deferred :: clone
    end type inode_t
    
    type, extends(inode_t) :: file_t
    character(:), allocatable :: name
    contains
    procedure :: print => file_t_print
    procedure :: clone => file_t_clone
    end type file_t
    
    !> Wrapper (Important)
    type node_t
    class(inode_t), pointer :: inode
    end type node_t
    
    type, extends(inode_t) :: folder_t
    type(node_t), allocatable :: children(:)
    character(:), allocatable :: name
    contains
    procedure :: print => folder_t_print
    procedure :: clone => folder_t_clone
    end type folder_t
    
    abstract interface
    
        subroutine inode_t_print(self, indentation)
        import inode_t
        class(inode_t), intent(inout) :: self
        character(*), intent(in) :: indentation
        end subroutine inode_t_print

        function inode_t_clone(self) result(inode)
        import inode_t
        class(inode_t), intent(inout) :: self
        class(inode_t), allocatable :: inode
        end function inode_t_clone
    
    end interface

contains

    subroutine file_t_print(self, indentation)
    class(file_t), intent(inout) :: self
    character(*), intent(in) :: indentation
        print *, indentation // self%name
    end subroutine file_t_print

    function file_t_clone(self) result(inode)
    class(file_t), intent(inout) :: self
    class(inode_t), allocatable :: inode
        allocate(file_t :: inode)
        inode = file_t(name=self%name // "_clone")
    end function file_t_clone
    
    ! - - - - - - - - -
    
    subroutine folder_t_print(self, indentation)
    class(folder_t), intent(inout) :: self
    character(*), intent(in) :: indentation
    integer :: i
        print *, indentation // self%name
        if (size(self%children) == 0) return
        do i = 1, size(self%children)
        call self%children(i)%inode%print(indentation // indentation)
        end do
    end subroutine folder_t_print

    !> There may be incorrect usage here, but I have no choice but to do so. 
    !>  Fortran's compilation check is stricter, and I am indeed bypassing it.
    function folder_t_clone(self) result(inode)
    class(folder_t), intent(inout) :: self
    class(inode_t), allocatable :: inode
    type(folder_t), allocatable :: tmp_folder
    integer :: i
        allocate(tmp_folder, source=self)
        tmp_folder%name = tmp_folder%name // "_clone"
        if (size(self%children) > 0) then
        do i = 1, size(tmp_folder%children)
            associate(node => tmp_folder%children(i)%inode)
            allocate(node, source=node%clone())
            end associate
        end do
        end if
        allocate(inode, source=tmp_folder)
    end function folder_t_clone

end module prototype_module