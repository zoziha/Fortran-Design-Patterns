module cache_module

    implicit none
    private

    public :: cache_t, cache_factory_t

    type, abstract :: cache_t
    contains
        procedure(cache_t_operation), deferred :: operation
    end type cache_t

    abstract interface
        subroutine cache_t_operation(self)
            import cache_t
            class(cache_t), intent(inout) :: self
        end subroutine cache_t_operation
    end interface

    type, extends(cache_t) :: concrete_cache_t
        character(:), allocatable :: key
    contains
        procedure :: operation => concrete_cache_t_operation
    end type concrete_cache_t

    type node_t
        class(cache_t), allocatable :: cache
    end type node_t

    type cache_factory_t
        type(node_t), allocatable :: cache_list(:)
    contains
        procedure :: get_cache => cache_factory_t_get_cache
    end type cache_factory_t

contains

    subroutine concrete_cache_t_operation(self)
        class(concrete_cache_t), intent(inout) :: self
        print *, self%key
    end subroutine concrete_cache_t_operation

    function cache_factory_t_get_cache(self, key) result(cache)
        class(cache_factory_t), intent(inout), target :: self
        character(*), intent(in) :: key
        class(cache_t), pointer :: cache
        integer :: i

        if (allocated(self%cache_list)) then
            do i = 1, size(self%cache_list)
                associate (cache_ => self%cache_list(i)%cache)
                
                    select type (cache_)
                    type is (concrete_cache_t)
                        if (cache_%key == key) then
                            cache => self%cache_list(i)%cache
                            return
                        end if
                    end select
                    
                end associate
            end do
        end if

        self%cache_list = append_slice(self%cache_list, key)
        cache => self%cache_list(size(self%cache_list))%cache

    end function cache_factory_t_get_cache

    !> Date structure
    function append_slice(cache_list_in, key) result(cache_list_out)
        type(node_t), intent(inout), allocatable :: cache_list_in(:)
        character(*), intent(in) :: key
        type(node_t), allocatable :: cache_list_out(:)
        integer :: i

        if (.not. allocated(cache_list_in)) then
            allocate (cache_list_out(1))
            allocate (cache_list_out(1)%cache, source=concrete_cache_t(key=key))
        else
            i = size(cache_list_in)
            allocate (cache_list_out(i + 1))
            cache_list_out(1:i) = cache_list_in
            allocate (cache_list_out(i + 1)%cache, source=concrete_cache_t(key=key))
        end if
    end function append_slice

end module cache_module
