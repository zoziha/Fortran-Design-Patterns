module mediator_module

    implicit none
    private

    public :: station_manager_t, passenger_train_t, freight_train_t

    type, abstract :: train_t
    contains
        procedure(train_t_arrive), deferred :: arrive
        procedure(train_t_depart), deferred :: depart
        procedure(train_t_permit_arrival), deferred :: permit_arrival
    end type train_t

    type, abstract :: mediator_t
    contains
        procedure(mediator_t_can_arrive), deferred :: can_arrive
        procedure(mediator_t_notify_about_departure), deferred :: notify_about_departure
    end type mediator_t

    abstract interface

        subroutine train_t_arrive(self)
            import train_t
            class(train_t), intent(inout) :: self
        end subroutine train_t_arrive

        subroutine train_t_depart(self)
            import train_t
            class(train_t), intent(inout) :: self
        end subroutine train_t_depart

        subroutine train_t_permit_arrival(self)
            import train_t
            class(train_t), intent(inout) :: self
        end subroutine train_t_permit_arrival

        logical function mediator_t_can_arrive(self, train) result(can)
            import mediator_t, train_t
            class(mediator_t), intent(inout) :: self
            class(train_t), intent(in), target :: train
        end function mediator_t_can_arrive

        subroutine mediator_t_notify_about_departure(self)
            import mediator_t
            class(mediator_t), intent(inout) :: self
        end subroutine mediator_t_notify_about_departure

    end interface

    type, extends(train_t) :: passenger_train_t
        class(mediator_t), pointer :: mediator
    contains
        procedure :: arrive => passenger_train_t_arrive
        procedure :: depart => passenger_train_t_depart
        procedure :: permit_arrival => passenger_train_t_permit_arrival
    end type passenger_train_t

    type, extends(train_t) :: freight_train_t
        class(mediator_t), pointer :: mediator
    contains
        procedure :: arrive => freight_train_t_arrive
        procedure :: depart => freight_train_t_depart
        procedure :: permit_arrival => freight_train_t_permit_arrival
    end type freight_train_t

    type node_t
        class(train_t), pointer :: train
    end type node_t

    type, extends(mediator_t) :: station_manager_t
        logical :: is_platform_free = .true.
        type(node_t), allocatable :: list(:)
    contains
        procedure :: can_arrive => station_manager_t_can_arrive
        procedure :: notify_about_departure => station_manager_t_notify_about_departure
    end type station_manager_t

contains

    subroutine passenger_train_t_arrive(self)
        class(passenger_train_t), intent(inout) :: self
        if (.not. self%mediator%can_arrive(self)) then
            print *, "Passenger train: arrival blocked, waiting"
            return
        end if
        print *, "Passenger train: arrived"
    end subroutine passenger_train_t_arrive

    subroutine passenger_train_t_depart(self)
        class(passenger_train_t), intent(inout) :: self
        print *, "Passenger train: leaving"
        call self%mediator%notify_about_departure()
    end subroutine passenger_train_t_depart

    subroutine passenger_train_t_permit_arrival(self)
        class(passenger_train_t), intent(inout) :: self
        print *, "Passenger train: arrival permitted, arriving"
        call self%arrive()
    end subroutine passenger_train_t_permit_arrival

    subroutine freight_train_t_arrive(self)
        class(freight_train_t), intent(inout) :: self
        
        if (.not. self%mediator%can_arrive(self)) then
            print *, "Freight train: arrival blocked, waiting"
            return
        end if
        print *, "Freight train: arrived"
        
    end subroutine freight_train_t_arrive

    subroutine freight_train_t_depart(self)
        class(freight_train_t), intent(inout) :: self
        print *, "freight train: leaving"
        call self%mediator%notify_about_departure()
    end subroutine freight_train_t_depart

    subroutine freight_train_t_permit_arrival(self)
        class(freight_train_t), intent(inout) :: self
        print *, "Freight train: arrival permitted, arriving"
        call self%arrive()
    end subroutine freight_train_t_permit_arrival

    logical function station_manager_t_can_arrive(self, train) result(can)
        class(station_manager_t), intent(inout) :: self
        class(train_t), intent(in), target :: train
        
        if (self%is_platform_free) then
            self%is_platform_free = .false.
            can = .true.
            return
        end if
        self%list = [self%list, node_t(train)]
        can = .false.
        
    end function station_manager_t_can_arrive

    subroutine station_manager_t_notify_about_departure(self)
        class(station_manager_t), intent(inout) :: self
        class(train_t), pointer :: train
        
        if (.not. self%is_platform_free) then
            self%is_platform_free = .true.
        end if
        if (size(self%list) > 0) then
            train => self%list(1)%train
            !> 内存泄露
            self%list = self%list(2:)
            call train%permit_arrival()
        end if
        
    end subroutine station_manager_t_notify_about_departure

end module mediator_module
