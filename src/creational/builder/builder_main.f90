program builder_main
    use builder_module, only: ibuilder_type, director_type, house_type, get_builder
    implicit none

    class(ibuilder_type), allocatable :: normal_builder, igloo_builder
    type(director_type) :: director
    type(house_type) :: normal_house, igloo_house

    normal_builder = get_builder("normal")
    igloo_builder = get_builder("igloo")

    !> Normal House
    call director%set_builder(normal_builder)
    normal_house = director%build_house()

    print *, "Normal House Door Type: ", normal_house%door_type
    print *, "Normal House Window Type: ", normal_house%window_type
    print *, "Normal House Num Floor: ", normal_house%floor

    !> Igloo House
    call director%set_builder(igloo_builder)
    igloo_house = director%build_house()

    print *, "Igloo House Door Type: ", igloo_house%door_type
    print *, "Igloo House Window Type: ", igloo_house%window_type
    print *, "Igloo House Num Floor: ", igloo_house%floor

end program builder_main

!> Results shall be:

!  Normal House Door Type: Wooden Door
!  Normal House Window Type: Wooden Window
!  Normal House Num Floor:     2
!  Igloo House Door Type: Snow Door
!  Igloo House Window Type: Snow Window
!  Igloo House Num Floor:     1