module prototype_module

    implicit none
    private

    public :: file_type, folder_type, inode_type

    type, abstract :: inode_type
    contains
        procedure(inode_type_print), deferred :: print
        procedure(inode_type_clone), deferred :: clone
    end type inode_type

    type, extends(inode_type) :: file_type
        character(:), allocatable :: name
    contains
        procedure :: print => file_type_print
        procedure :: clone => file_type_clone
    end type file_type

    !> Wrapper (Important)
    type node_type
        class(inode_type), pointer :: inode
    end type node_type

    type, extends(inode_type) :: folder_type
        type(node_type), allocatable :: children(:)
        character(:), allocatable :: name
    contains
        procedure :: print => folder_type_print
        procedure :: clone => folder_type_clone
    end type folder_type

    abstract interface

        subroutine inode_type_print(self, indentation)
            import inode_type
            class(inode_type), intent(inout) :: self
            character(*), intent(in) :: indentation
        end subroutine inode_type_print

        function inode_type_clone(self) result(inode)
            import inode_type
            class(inode_type), intent(inout) :: self
            class(inode_type), allocatable :: inode
        end function inode_type_clone

    end interface

contains

    subroutine file_type_print(self, indentation)
        class(file_type), intent(inout) :: self
        character(*), intent(in) :: indentation
        print *, indentation//self%name
    end subroutine file_type_print

    function file_type_clone(self) result(inode)
        class(file_type), intent(inout) :: self
        class(inode_type), allocatable :: inode
        allocate (file_type :: inode)
        inode = file_type(name=self%name//"_clone")
    end function file_type_clone

    ! - - - - - - - - -

    subroutine folder_type_print(self, indentation)
        class(folder_type), intent(inout) :: self
        character(*), intent(in) :: indentation
        integer :: i
        print *, indentation//self%name
        if (size(self%children) == 0) return
        do i = 1, size(self%children)
            call self%children(i)%inode%print(indentation//indentation)
        end do
    end subroutine folder_type_print

    !> There may be incorrect usage here, but I have no choice but to do so.
    !>  Fortran's compilation check is stricter, and I am indeed bypassing it.
    function folder_type_clone(self) result(inode)
        class(folder_type), intent(inout) :: self
        class(inode_type), allocatable :: inode
        type(folder_type), allocatable :: tmp_folder
        integer :: i
        allocate (tmp_folder, source=self)
        tmp_folder%name = tmp_folder%name//"_clone"
        if (size(self%children) > 0) then
        do i = 1, size(tmp_folder%children)
            associate (node => tmp_folder%children(i)%inode)
                inode = node%clone()
                allocate (tmp_folder%children(i)%inode, source=inode)
            end associate
        end do
        end if
        
        call move_alloc(tmp_folder, inode)
    end function folder_type_clone

end module prototype_module
