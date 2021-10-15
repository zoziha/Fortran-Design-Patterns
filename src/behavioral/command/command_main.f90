!> Reference: https://refactoring.guru/design-patterns/command/go/example
program test_command

    use command_pattern, only: tv, on_command, off_command, button
    type(tv) :: t
    type(on_command) :: on_c
    type(off_command) :: off_c

    type(button) :: on_b
    type(button) :: off_b

    !> Linking
    allocate(on_c%d, source=t)
    allocate(off_c%d, source=t)

    allocate(on_b%c, source=on_c)
    allocate(off_b%c, source=off_c)

    !> Operating
    call on_b%press()
    call off_b%press()

    !> Free memory.
    deallocate(on_c%d)
    deallocate(off_c%d)
    deallocate(on_b%c)
    deallocate(off_b%c)

end program test_command

!> Results shall be:

!  Turning tv on. ✔️
!  Turning tv off. ❌