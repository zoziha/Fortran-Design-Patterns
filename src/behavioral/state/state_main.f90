program state_main

    use state_module, only: person_type
    implicit none

    type(person_type) :: person
    call person%hungry_state%no_hungry
    call person%work
    call person%work
    call person%eat
    call person%eat

end program state_main

!> Results shall be:

!  Ok, let us do work..
!  I am hungry, no work!!
!  Eatting..
!  Already baole!!

