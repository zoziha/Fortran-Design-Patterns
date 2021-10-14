program iterator_main

    use, intrinsic :: iso_fortran_env, only: int8
    use iterator_module, only: user_t, user_collection_t, user_iterator_t, iterator_t
    
    type(user_t) :: user1, user2, user
    type(user_collection_t) :: user_collection
    ! TODO:
    class(iterator_t), allocatable :: iterator
    
    user1 = user_t(name="A", age=30_int8)
    user2 = user_t(name="B", age=20_int8)
    
    user_collection = user_collection_t(users=[user1, user2])
    
    call user_collection%create_iterator(iterator)
    
    do while (iterator%has_next())
        user = iterator%get_next()
        print *, "User is ", user%name, user%age
    end do

end program iterator_main