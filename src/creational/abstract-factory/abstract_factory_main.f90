program abstract_factory_main

    use, intrinsic :: iso_fortran_env, only: int8
    use abstract_factory_module, only: isports_factory_type, erke_type, lining_type, get_sports_factory, erke_shoe_type, erke_shirt_type, &
                                       lining_shoe_type, lining_shirt_type, ishoe_type, ishirt_type

    class(isports_factory_type), allocatable :: erke_factory, lining_factory
    class(ishoe_type), allocatable :: erke_shoe
    class(ishirt_type), allocatable :: erke_shirt
    class(ishoe_type), allocatable :: lining_shoe
    class(ishirt_type), allocatable :: lining_shirt

    ! allocate (erke_t :: erke_factory)
    ! allocate (lining_t :: lining_factory)

    erke_factory = get_sports_factory("erke")
    lining_factory = get_sports_factory("lining")

    ! allocate (erke_shoe_t :: erke_shoe)
    ! allocate (erke_shirt_t :: erke_shirt)
    ! allocate (lining_shoe_t :: lining_shoe)
    ! allocate (lining_shirt_t :: lining_shirt)

    erke_shoe = erke_factory%make_shoe()
    erke_shirt = erke_factory%make_shirt()

    lining_shoe = lining_factory%make_shoe()
    lining_shirt = lining_factory%make_shirt()

    call print_shoe_details(erke_shoe)
    call print_shirt_details(erke_shirt)

    call print_shoe_details(lining_shoe)
    call print_shirt_details(lining_shirt)

contains

    subroutine print_shoe_details(ishoe)
        class(ishoe_type), intent(inout) :: ishoe

        print *, "This is a pair of shoes👟."
        print *, "Logo: ", ishoe%get_logo()
        print *, "Size: ", ishoe%get_size()

    end subroutine print_shoe_details

    subroutine print_shirt_details(ishirt)
        class(ishirt_type), intent(inout) :: ishirt

        print *, "This is a T-shirt👕."
        print *, "Logo: ", ishirt%get_logo()
        print *, "Size: ", ishirt%get_size()

    end subroutine print_shirt_details

end program abstract_factory_main

!> Results shall be:

!  This is a pair of shoes👟.
!  Logo: erke
!  Size:    14
!  This is a T-shirt👕.
!  Logo: erke
!  Size:    14
!  This is a pair of shoes👟.
!  Logo: lining
!  Size:    14
!  This is a T-shirt👕.
!  Logo: lining
!  Size:    14