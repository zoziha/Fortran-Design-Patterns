program iterator_main

    use, intrinsic :: iso_fortran_env, only: int8
    use iterator_module, only: user_type, user_collection_type, user_iterator_type, iterator_type

    type(user_type) :: user1, user2, user
    type(user_collection_type) :: user_collection
    ! TODO:
    class(iterator_type), allocatable :: iterator

    user1 = user_type(name="A", age=30_int8)
    user2 = user_type(name="B", age=20_int8)

    user_collection = user_collection_type(users=[user1, user2])

    !> Specific iterator
    allocate (user_iterator_type :: iterator)
    iterator = user_collection%create_iterator()

    do while (iterator%has_next())
        user = iterator%get_next()
        print "(3A,I3)", "User is ", user%name, ", age is ", user%age
    end do

    deallocate (iterator)

end program iterator_main

!> Results shall be:

!  User is A, age is  30
!  User is B, age is  20