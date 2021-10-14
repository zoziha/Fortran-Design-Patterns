module iterator_module
    
    use, intrinsic :: iso_fortran_env, only: int8
    implicit none
    private
    
    public :: user_t, user_collection_t, user_iterator_t, iterator_t
    
    !> Abstract types
    
    !> Collection
    type, abstract :: collection_t
    contains
        procedure(collection_t_create_iterator), deferred :: create_iterator
    end type collection_t
    
    !> Iterator
    type, abstract :: iterator_t
    contains
        procedure(iterator_t_has_next), deferred :: has_next
        procedure(iterator_t_get_next), deferred :: get_next
    end type iterator_t
    
    !> User
    type user_t
        character(:), allocatable :: name
        integer(int8) :: age
    end type user_t
    
    abstract interface
    
        function collection_t_create_iterator(self) result(iterator)
            import iterator_t, collection_t
            !> TODO:
            class(collection_t), intent(in) :: self
            class(iterator_t), allocatable :: iterator
        end function collection_t_create_iterator
        
        logical function iterator_t_has_next(self)
            import iterator_t
            class(iterator_t), intent(in) :: self
        end function iterator_t_has_next
        
        type(user_t) function iterator_t_get_next(self)
            import user_t, iterator_t
            class(iterator_t), intent(inout) :: self
        end function iterator_t_get_next
        
    end interface
    
    !> Specific types
    
    !> User collection
    type, extends(collection_t) :: user_collection_t
        type(user_t), allocatable :: users(:)
    contains
        procedure :: create_iterator => user_collection_t_create_iterator
    end type user_collection_t
    
    !> User iterator
    type, extends(iterator_t) :: user_iterator_t
        integer :: index
        type(user_t), allocatable :: users(:)
    contains
        procedure :: has_next => user_iterator_t_has_next
        procedure :: get_next => user_iterator_t_get_next
    end type user_iterator_t

contains

    function user_collection_t_create_iterator(self) result(iterator)
        class(user_collection_t), intent(in) :: self
        class(iterator_t), allocatable :: iterator
        ! TODO:
        iterator = user_iterator_t(index=0, users=self%users)
    end function user_collection_t_create_iterator
    
    logical function user_iterator_t_has_next(self) result(has)
        class(user_iterator_t), intent(in) :: self
        
        has = merge(.true., .false., self%index < size(self%users))
        
    end function user_iterator_t_has_next
    
    type(user_t) function user_iterator_t_get_next(self) result(user)
        class(user_iterator_t), intent(inout) :: self
        
        self%index = self%index + 1
        user = self%users(self%index)
    
    end function user_iterator_t_get_next
    
end module iterator_module