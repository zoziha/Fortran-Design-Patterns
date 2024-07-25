!> CoR: Hospital departments
module hospital_CoR

    implicit none
    private

    public :: patient_type, department_type, reception_type, doctor_type, medical_type, cashier_type

    type patient_type
        character(:), allocatable :: name
        logical :: registration_done
        logical :: doctor_check_up_done
        logical :: medicine_done
        logical :: payment_done
    end type patient_type

    type, abstract :: department_type
    contains
        procedure(execute_procedure), deferred :: execute
        procedure(set_next_procedure), deferred :: set_next
    end type department_type

    abstract interface
        subroutine execute_procedure(self, p)
            import department_type, patient_type
            class(department_type), intent(inout) :: self
            type(patient_type), intent(inout) :: p
        end subroutine execute_procedure
        subroutine set_next_procedure(self, next)
            import department_type
            class(department_type), intent(inout) :: self
            class(department_type), intent(inout) :: next
        end subroutine set_next_procedure
    end interface

    type, extends(department_type) :: reception_type
        class(department_type), pointer :: next
    contains
        procedure :: execute => reception_type_execute
        procedure :: set_next => reception_type_set_next
    end type reception_type

    type, extends(department_type) :: doctor_type
        class(department_type), pointer :: next
    contains
        procedure :: execute => doctor_type_execute
        procedure :: set_next => doctor_type_set_next
    end type doctor_type

    type, extends(department_type) :: medical_type
        class(department_type), pointer :: next
    contains
        procedure :: execute => medicine_type_execute
        procedure :: set_next => medicine_type_set_next
    end type medical_type

    type, extends(department_type) :: cashier_type
        class(department_type), pointer :: next
    contains
        procedure :: execute => cashier_type_execute
        procedure :: set_next => cashier_type_set_next
    end type cashier_type

contains

    subroutine reception_type_execute(self, p)
        class(reception_type), intent(inout) :: self
        type(patient_type), intent(inout) :: p

        if (p%registration_done) then
            print *, "Patient registration already done.✔️"
            call self%next%execute(p)
            return
        end if

        print *, "Reception registering patient."
        p%registration_done = .true.
        call self%next%execute(p)

    end subroutine reception_type_execute

    subroutine reception_type_set_next(self, next)
        class(reception_type), intent(inout) :: self
        class(department_type), intent(inout) :: next

        allocate (self%next, source=next)

    end subroutine reception_type_set_next

    subroutine doctor_type_execute(self, p)
        class(doctor_type), intent(inout) :: self
        type(patient_type), intent(inout) :: p

        if (p%doctor_check_up_done) then
            print *, "Doctor checkup already done.✔️"
            call self%next%execute(p)
            return
        end if

        print *, "Doctor checking patient."
        p%doctor_check_up_done = .true.
        call self%next%execute(p)

    end subroutine doctor_type_execute

    subroutine doctor_type_set_next(self, next)
        class(doctor_type), intent(inout) :: self
        class(department_type), intent(inout) :: next

        allocate (self%next, source=next)

    end subroutine doctor_type_set_next

    subroutine medicine_type_execute(self, p)
        class(medical_type), intent(inout) :: self
        type(patient_type), intent(inout) :: p

        if (p%medicine_done) then
            print *, "Medicine already given to patient.✔️"
            call self%next%execute(p)
            return
        end if

        print *, "Medical giving medicine to patient."
        p%medicine_done = .true.
        call self%next%execute(p)

    end subroutine medicine_type_execute

    subroutine medicine_type_set_next(self, next)
        class(medical_type), intent(inout) :: self
        class(department_type), intent(inout) :: next

        allocate (self%next, source=next)

    end subroutine medicine_type_set_next

    subroutine cashier_type_execute(self, p)
        class(cashier_type), intent(inout) :: self
        type(patient_type), intent(inout) :: p

        if (p%payment_done) then
            print *, "Payment Done.✔️"
            return
        end if

        print *, "Cashier getting money from patient."
        p%payment_done = .true.

    end subroutine cashier_type_execute

    subroutine cashier_type_set_next(self, next)
        class(cashier_type), intent(inout) :: self
        class(department_type), intent(inout) :: next

        allocate (self%next, source=next)

    end subroutine cashier_type_set_next

end module hospital_CoR
