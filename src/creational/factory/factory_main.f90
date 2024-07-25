program factory_main

    use factory_module, only: igun_type, ak47_type, musket_type, get_gun
    implicit none

    class(igun_type), allocatable :: ak47, musket

    allocate (ak47_type :: ak47)
    allocate (musket_type :: musket)

    ak47 = get_gun("ak47")
    musket = get_gun("musket")

    call print_details(ak47)
    call print_details(musket)

contains

    subroutine print_details(igun)
        class(igun_type), intent(inout) :: igun
        print *, "Gun: ", igun%get_name()
        print *, "Power: ", igun%get_power()
    end subroutine print_details

end program factory_main

!> Results shall be:

!  Gun: ak47 gun
!  Power:     4
!  Gun: musket gun
!  Power:     1