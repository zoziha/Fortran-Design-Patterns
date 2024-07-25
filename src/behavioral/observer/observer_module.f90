!> Reference: https://refactoring.guru/design-patterns/observer/go/example
module observer_pattern

    implicit none
    private

    public :: item_type, customer_type, new_item

    !> Abstract classes
    type, abstract :: subject_type
    contains
        procedure(register_procedure), deferred :: register
        procedure(deregister_procedure), deferred :: deregister
        procedure(notify_all_procedure), deferred :: notify_all
    end type subject_type

    type, abstract :: observer_type
    contains
        procedure(update_procedure), deferred :: update
        procedure(get_ID_procedure), deferred :: get_ID
    end type observer_type

    !> We cannot directly use `class(observer), allocatable :: o_list(:)`
    !>  instead of `type(node), allocatable :: o_list(:)`.
    type node_type
        class(observer_type), allocatable :: o
    end type node_type

    abstract interface
        subroutine register_procedure(self, o)
            import subject_type, observer_type
            class(subject_type), intent(inout) :: self
            class(observer_type), intent(inout) :: o
        end subroutine register_procedure
        subroutine deregister_procedure(self, o)
            import subject_type, observer_type
            class(subject_type), intent(inout) :: self
            class(observer_type), intent(inout) :: o
        end subroutine deregister_procedure
        subroutine notify_all_procedure(self)
            import subject_type
            class(subject_type), intent(inout) :: self
        end subroutine notify_all_procedure
        subroutine update_procedure(self, s)
            import observer_type
            class(observer_type), intent(inout) :: self
            character(len=*), intent(inout) :: s
        end subroutine update_procedure
        function get_ID_procedure(self) result(result)
            import observer_type
            class(observer_type), intent(inout) :: self
            character(len=:), allocatable :: result
        end function get_ID_procedure
    end interface

    !> Specific objects

    type, extends(subject_type) :: item_type
        type(node_type), allocatable :: o_list(:)
        character(len=:), allocatable :: name
        logical :: in_stock
    contains
        procedure :: update_availability
        procedure :: register
        procedure :: deregister
        procedure :: notify_all
    end type item_type

    type, extends(observer_type) :: customer_type
        character(len=:), allocatable :: ID
    contains
        procedure :: update
        procedure :: get_ID
    end type customer_type

contains

    !> Constructor of `item`.
    function new_item(name) result(i)
        character(*), intent(in) :: name
        type(item_type) :: i
        i%name = name
    end function new_item

    !> Remove a object from the subscription array.
    function remove_from_slice(o_list, o_to_remove) result(result)
        type(node_type), intent(inout) :: o_list(:)
        class(observer_type), intent(inout) :: o_to_remove
        type(node_type), allocatable :: result(:)
        character(len=:), allocatable :: id
        integer :: i, j
        i = size(o_list)
        id = o_to_remove%get_ID()
        do j = 1, i
            if (o_list(j)%o%get_ID() == id) then
                allocate (result(i - 1), source=[o_list(:j - 1), o_list(j + 1:)])
                return
            end if
        end do
        result = o_list
    end function remove_from_slice

    !> Append a object to the subscription array.
    function append_slice(o_list, o_to_append) result(result)
        type(node_type), intent(inout), allocatable :: o_list(:)
        class(observer_type), intent(inout) :: o_to_append
        type(node_type), allocatable :: result(:)
        integer :: i
        if (.not. allocated(o_list)) then
            allocate (result(1))
            allocate (result(1)%o, source=o_to_append)
        else
            i = size(o_list)
            allocate (result(i + 1))
            result(1:i) = o_list
            allocate (result(i + 1)%o, source=o_to_append)
        end if
    end function append_slice

    subroutine update_availability(self)
        class(item_type), intent(inout) :: self
        print *, "> Item "//self%name//" 👔 is now in stock."
        self%in_stock = .true.
        call self%notify_all()
    end subroutine update_availability

    subroutine register(self, o)
        class(item_type), intent(inout) :: self
        class(observer_type), intent(inout) :: o
        self%o_list = append_slice(self%o_list, o)
    end subroutine register

    subroutine deregister(self, o)
        class(item_type), intent(inout) :: self
        class(observer_type), intent(inout) :: o
        self%o_list = remove_from_slice(self%o_list, o)
    end subroutine deregister

    subroutine notify_all(self)
        class(item_type), intent(inout) :: self
        integer :: i
        do i = 1, size(self%o_list)
            call self%o_list(i)%o%update(self%name)
        end do
    end subroutine notify_all

    subroutine update(self, s)
        class(customer_type), intent(inout) :: self
        character(len=*), intent(inout) :: s
        print *, "Sending email to customer "//self%ID//" 📨 for item "//s//"."
    end subroutine update

    function get_ID(self) result(result)
        class(customer_type), intent(inout) :: self
        character(len=:), allocatable :: result
        result = self%ID
    end function get_ID

end module observer_pattern
