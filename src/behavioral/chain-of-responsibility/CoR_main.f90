
!> CoR: Patient visiting hospital
program CoR_main

    use hospital_CoR

    type(cashier) :: c
    type(medical) :: m
    type(doctor) :: d
    type(reception) :: r

    type(patient) :: p1, p2

    !> Set next for departments
    call m%set_next(c)
    call d%set_next(m)
    call r%set_next(d)

    p1 = patient("abc", .true., .true., .true., .true.)
    !> Patient visiting
    print *, "> Patient `"//p1%name//"` : "
    call r%execute(p1)

    p2 = patient("def", .true., .false., .false., .false.)
    !> Patient visiting
    print *, "> Patient `"//p2%name//"` : "
    call r%execute(p2)

    !> Optional statements
    deallocate (m%next)
    deallocate (d%next)
    deallocate (r%next)

end program CoR_main

!> Results shall be:

!  > Patient `abc` :
!  Patient registration already done.✔️
!  Doctor checkup already done.✔️
!  Medicine already given to patient.✔️
!  Payment Done.✔️
!  > Patient `def` :
!  Patient registration already done.✔️
!  Doctor checking patient.
!  Medical giving medicine to patient.
!  Cashier getting money from patient.