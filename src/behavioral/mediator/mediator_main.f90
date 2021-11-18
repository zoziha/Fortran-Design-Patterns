program mediator_main

    use mediator_module, only: station_manager_t,passenger_train_t,freight_train_t
    implicit none
    type(station_manager_t), target :: station_manager
    type(passenger_train_t) :: passenger_train
    type(freight_train_t) :: freight_train
    
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