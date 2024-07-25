program cache_main

    use cache_module, only: cache_factory_type, cache_type
    implicit none
    type(cache_factory_type) factory
    class(cache_type), pointer :: cache

    cache => factory%get_cache("A")
    call cache%operation()

    cache => factory%get_cache("A")
    call cache%operation()

    cache => factory%get_cache("B")
    call cache%operation()

    cache => factory%get_cache("C")
    call cache%operation()

    print *, "List length: ", size(factory%cache_list)

end program cache_main

!> Results shall be:

!  A
!  A
!  B
!  C
!  List length:            3
