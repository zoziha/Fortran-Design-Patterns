module iterator_module

    use, intrinsic :: iso_fortran_env, only: int8
    implicit none
    private

    public :: user_type, user_collection_type, user_iterator_type, iterator_type

    !> Abstract types

    !> Collection
    type, abstract :: collection_type
    contains
        procedure(collection_type_create_iterator), deferred :: create_iterator
    end type collection_type

    !> Iterator
    type, abstract :: iterator_type
    contains
        procedure(iterator_t_has_next), deferred :: has_next
        procedure(iterator_t_get_next), deferred :: get_next
    end type iterator_type

    !> User
    type user_type
        character(:), allocatable :: name
        integer(int8) :: age
    end type user_type

    abstract interface

        function collection_type_create_iterator(self) result(iterator)
            import iterator_type, collection_type
            !> TODO:
            class(collection_type), intent(in) :: self
            class(iterator_type), allocatable :: iterator
        end function collection_type_create_iterator

        logical function iterator_t_has_next(self)
            import iterator_type
            class(iterator_type), intent(in) :: self
        end function iterator_t_has_next

        type(user_type) function iterator_t_get_next(self)
            import user_type, iterator_type
            class(iterator_type), intent(inout) :: self
        end function iterator_t_get_next

    end interface

    !> Specific types

    !> User collection
    type, extends(collection_type) :: user_collection_type
        type(user_type), allocatable :: users(:)
    contains
        procedure :: create_iterator => user_collection_t_create_iterator
    end type user_collection_type

    !> User iterator
    type, extends(iterator_type) :: user_iterator_type
        integer :: index
        type(user_type), allocatable :: users(:)
    contains
        procedure :: has_next => user_iterator_t_has_next
        procedure :: get_next => user_iterator_t_get_next
    end type user_iterator_type

contains

    function user_collection_t_create_iterator(self) result(iterator)
        class(user_collection_type), intent(in) :: self
        class(iterator_type), allocatable :: iterator
        ! TODO:
        iterator = user_iterator_type(index=0, users=self%users)
    end function user_collection_t_create_iterator

    logical function user_iterator_t_has_next(self) result(has)
        class(user_iterator_type), intent(in) :: self

        has = merge(.true., .false., self%index < size(self%users))

    end function user_iterator_t_has_next

    type(user_type) function user_iterator_t_get_next(self) result(user)
        class(user_iterator_type), intent(inout) :: self

        self%index = self%index + 1
        user = self%users(self%index)

    end function user_iterator_t_get_next

end module iterator_module
