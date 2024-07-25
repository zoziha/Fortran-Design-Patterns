program mediator_main

    use mediator_module, only: station_manager_type,passenger_train_type,freight_train_type
    implicit none
    type(station_manager_type), target :: station_manager
    type(passenger_train_type) :: passenger_train
    type(freight_train_type) :: freight_train
    
    allocate(station_manager%list(0))
    passenger_train%mediator => station_manager
    freight_train%mediator => station_manager
    
    call passenger_train%arrive()
    call freight_train%arrive()
    call passenger_train%depart()
    
end program mediator_main

!> Results shall be:

!  Passenger train: arrived
!  Freight train: arrival blocked, waiting
!  Passenger train: leaving
!  Freight train: arrival permitted, arriving
!  Freight train: arrived