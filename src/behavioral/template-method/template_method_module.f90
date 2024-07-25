module template_method_module

    implicit none
    private

    public :: otp_type, sms_type, email_type

    type, abstract :: iopt_type
    contains
        procedure(iopt_type_gen_random_opt), deferred :: gen_random_opt
        procedure(iopt_type_save_opt_cache), deferred :: save_opt_cache
        procedure(iopt_type_get_message), deferred :: get_message
        procedure(iopt_type_send_notification), deferred :: send_notification
        procedure(iopt_type_publish_metric), deferred :: publish_metric
    end type iopt_type

    abstract interface

        function iopt_type_gen_random_opt(self, len) result(random_opt)
            import iopt_type
            class(iopt_type), intent(inout) :: self
            integer, intent(in) :: len
            character(:), allocatable :: random_opt
        end function iopt_type_gen_random_opt

        subroutine iopt_type_save_opt_cache(self, otp)
            import iopt_type
            class(iopt_type), intent(inout) :: self
            character(*), intent(inout) :: otp
        end subroutine iopt_type_save_opt_cache

        function iopt_type_get_message(self, otp) result(msg)
            import iopt_type
            class(iopt_type), intent(inout) :: self
            character(*), intent(inout) :: otp
            character(:), allocatable :: msg
        end function iopt_type_get_message

        subroutine iopt_type_send_notification(self, msg)
            import iopt_type
            class(iopt_type), intent(inout) :: self
            character(*), intent(inout) :: msg
        end subroutine iopt_type_send_notification

        subroutine iopt_type_publish_metric(self)
            import iopt_type
            class(iopt_type), intent(inout) :: self
        end subroutine iopt_type_publish_metric

    end interface

    ! - - - - - - - - - - - - -

    type otp_type
        class(iopt_type), pointer :: iopt
    contains
        procedure :: gen_and_send_otp => otp_type_gen_and_send_otp
    end type otp_type

    type, extends(iopt_type) :: sms_type
    contains
        procedure :: gen_random_opt => sms_type_gen_random_opt
        procedure :: save_opt_cache => sms_type_save_opt_cache
        procedure :: get_message => sms_type_get_message
        procedure :: send_notification => sms_type_send_notification
        procedure :: publish_metric => sms_type_publish_metric
    end type sms_type

    type, extends(iopt_type) :: email_type
    contains
        procedure :: gen_random_opt => email_type_gen_random_opt
        procedure :: save_opt_cache => email_type_save_opt_cache
        procedure :: get_message => email_type_get_message
        procedure :: send_notification => email_type_send_notification
        procedure :: publish_metric => email_type_publish_metric
    end type email_type

contains

    subroutine otp_type_gen_and_send_otp(self, otp_length)
        class(otp_type), intent(inout) :: self
        integer, intent(in) :: otp_length

        character(:), allocatable :: otp
        character(:), allocatable :: msg

        otp = self%iopt%gen_random_opt(otp_length)
        call self%iopt%save_opt_cache(otp)
        msg = self%iopt%get_message(otp)
        call self%iopt%send_notification(msg)
        call self%iopt%publish_metric()

    end subroutine otp_type_gen_and_send_otp

    ! - - - - - - - - - -

    function sms_type_gen_random_opt(self, len) result(random_opt)
        class(sms_type), intent(inout) :: self
        integer, intent(in) :: len
        character(:), allocatable :: random_opt

        random_opt = "1234"
        print *, "SMS: generating random otp ", random_opt

    end function sms_type_gen_random_opt

    subroutine sms_type_save_opt_cache(self, otp)
        class(sms_type), intent(inout) :: self
        character(*), intent(inout) :: otp

        print *, "SMS: saving otp: ", otp, " to cache"

    end subroutine sms_type_save_opt_cache

    function sms_type_get_message(self, otp) result(msg)
        class(sms_type), intent(inout) :: self
        character(*), intent(inout) :: otp
        character(:), allocatable :: msg

        msg = "SMS OTP for login is "//otp

    end function sms_type_get_message

    subroutine sms_type_send_notification(self, msg)
        class(sms_type), intent(inout) :: self
        character(*), intent(inout) :: msg

        print *, "SMS: sending sms: "//msg

    end subroutine sms_type_send_notification

    subroutine sms_type_publish_metric(self)
        class(sms_type), intent(inout) :: self

        print *, "SMS: publishing metric"

    end subroutine sms_type_publish_metric

    ! - - - - - - - - - -

    function email_type_gen_random_opt(self, len) result(random_opt)
        class(email_type), intent(inout) :: self
        integer, intent(in) :: len
        character(:), allocatable :: random_opt

        random_opt = "1234"
        print *, "EMAIL: generating random otp ", random_opt

    end function email_type_gen_random_opt

    subroutine email_type_save_opt_cache(self, otp)
        class(email_type), intent(inout) :: self
        character(*), intent(inout) :: otp

        print *, "EMAIL: saving otp: ", otp, " to cache"

    end subroutine email_type_save_opt_cache

    function email_type_get_message(self, otp) result(msg)
        class(email_type), intent(inout) :: self
        character(*), intent(inout) :: otp
        character(:), allocatable :: msg

        msg = "EMAIL OTP for login is "//otp

    end function email_type_get_message

    subroutine email_type_send_notification(self, msg)
        class(email_type), intent(inout) :: self
        character(*), intent(inout) :: msg

        print *, "EMAIL: sending email: "//msg

    end subroutine email_type_send_notification

    subroutine email_type_publish_metric(self)
        class(email_type), intent(inout) :: self

        print *, "EMAIL: publishing metric"

    end subroutine email_type_publish_metric

end module template_method_module
