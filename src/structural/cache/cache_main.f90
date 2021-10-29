program cache_main

    use cache_module, only: cache_factory_t, cache_t
    implicit none
    type(cache_factory_t) factory
    class(cache_t), pointer :: cache

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
