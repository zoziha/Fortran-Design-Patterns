program memento_main

    use memento_module, only: caretaker_t, originator_t

    type(caretaker_t) :: caretaker
    type(originator_t) :: originator

    allocate (caretaker%memento(0))
    originator%state = "A"

    print *, "Originator state: ", originator%get_state()
    call caretaker%add_memento(originator%create_memento())

    call originator%set_state("B")
    print *, "Originator current state: ", originator%get_state()
    call caretaker%add_memento(originator%create_memento())

    call originator%set_state("C")
    print *, "Originator current state: ", originator%get_state()
    call caretaker%add_memento(originator%create_memento())

    call originator%restore_memento(caretaker%get_memento(2))
    print *, "Restored to state: ", originator%get_state()

    call originator%restore_memento(caretaker%get_memento(1))
    print *, "Restored to state: ", originator%get_state()

end program memento_main

!> Results shall be:

!  Originator state: A
!  Originator current state: B
!  Originator current state: C
!  Restored to state: B
!  Restored to state: A
