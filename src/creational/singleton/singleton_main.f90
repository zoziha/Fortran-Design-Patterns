program singleton_main

    use singleton_module, only: single, get_instance, dispose_instance
    implicit none

    single = get_instance(10)
    single = get_instance(23)
    single = get_instance(0)
    call dispose_instance(single)
    single = get_instance(9)

end program singleton_main
