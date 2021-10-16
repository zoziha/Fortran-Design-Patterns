module template_method_module

    implicit none
    private
    
    public :: otp_t, sms_t, email_t
    
    type, abstract :: iopt_t
    contains
    procedure(iopt_t_gen_random_opt), deferred :: gen_random_opt
    procedure(iopt_t_save_opt_cache), deferred :: save_opt_cache
    procedure(iopt_t_get_message)   , deferred :: get_message
    procedure(iopt_t_send_notification), deferred :: send_notification
    procedure(iopt_t_publish_metric), deferred :: publish_metric
    end type iopt_t
    
    abstract interface
    
        function iopt_t_gen_random_opt(self, len) result(random_opt)
        import iopt_t
        class(iopt_t), intent(inout) :: self
        integer, intent(in) :: len
        character(:), allocatable :: random_opt 
        end function iopt_t_gen_random_opt
        
        subroutine iopt_t_save_opt_cache(self, otp)
        import iopt_t
        class(iopt_t), intent(inout) :: self
        character(*), intent(inout) :: otp
        end subroutine iopt_t_save_opt_cache
        
        function iopt_t_get_message(self, otp) result(msg)
        import iopt_t
        class(iopt_t), intent(inout) :: self
        character(*), intent(inout) :: otp
        character(:), allocatable :: msg
        end function iopt_t_get_message
        
        subroutine iopt_t_send_notification(self, msg)
        import iopt_t
        class(iopt_t), intent(inout) :: self
        character(*), intent(inout) :: msg
        end subroutine iopt_t_send_notification
        
        subroutine iopt_t_publish_metric(self)
        import iopt_t
        class(iopt_t), intent(inout) :: self
        end subroutine iopt_t_publish_metric
        
    end interface
    
    ! - - - - - - - - - - - - -
    
    type otp_t
    class(iopt_t), pointer :: iopt
    contains
    procedure :: gen_and_send_otp => otp_t_gen_and_send_otp
    end type otp_t
    
    type, extends(iopt_t) :: sms_t
    contains
    procedure :: gen_random_opt => sms_t_gen_random_opt
    procedure :: save_opt_cache => sms_t_save_opt_cache
    procedure :: get_message    => sms_t_get_message
    procedure :: send_notification => sms_t_send_notification
    procedure :: publish_metric => sms_t_publish_metric
    end type sms_t
    
    type, extends(iopt_t) :: email_t
    contains
    procedure :: gen_random_opt => email_t_gen_random_opt
    procedure :: save_opt_cache => email_t_save_opt_cache
    procedure :: get_message    => email_t_get_message
    procedure :: send_notification => email_t_send_notification
    procedure :: publish_metric => email_t_publish_metric
    end type email_t
    
contains

    subroutine otp_t_gen_and_send_otp(self, otp_length)
    class(otp_t), intent(inout) :: self
    integer, intent(in) :: otp_length
    
    character(:), allocatable :: otp
    character(:), allocatable :: msg
    
    otp = self%iopt%gen_random_opt(otp_length)
    call self%iopt%save_opt_cache(otp)
    msg = self%iopt%get_message(otp)
    call self%iopt%send_notification(msg)
    call self%iopt%publish_metric()
        
    end subroutine otp_t_gen_and_send_otp
    
    ! - - - - - - - - - -
    
    function sms_t_gen_random_opt(self, len) result(random_opt)
    class(sms_t), intent(inout) :: self
    integer, intent(in) :: len
    character(:), allocatable :: random_opt
    
    random_opt = "1234"
    print *, "SMS: generating random otp ", random_opt
        
    end function sms_t_gen_random_opt
    
    subroutine sms_t_save_opt_cache(self, otp)
    class(sms_t), intent(inout) :: self
    character(*), intent(inout) :: otp
    
    print *, "SMS: saving otp: ", otp, " to cache"
        
    end subroutine sms_t_save_opt_cache
    
    function sms_t_get_message(self, otp) result(msg)
    class(sms_t), intent(inout) :: self
    character(*), intent(inout) :: otp
    character(:), allocatable :: msg
    
    msg = "SMS OTP for login is " // otp
        
    end function sms_t_get_message
    
    subroutine sms_t_send_notification(self, msg)
    class(sms_t), intent(inout) :: self
    character(*), intent(inout) :: msg
    
    print *, "SMS: sending sms: " // msg
        
    end subroutine sms_t_send_notification
    
    subroutine sms_t_publish_metric(self)
    class(sms_t), intent(inout) :: self
    
    print *, "SMS: publishing metric"
        
    end subroutine sms_t_publish_metric
    
    ! - - - - - - - - - -
    
    function email_t_gen_random_opt(self, len) result(random_opt)
    class(email_t), intent(inout) :: self
    integer, intent(in) :: len
    character(:), allocatable :: random_opt
    
    random_opt = "1234"
    print *, "EMAIL: generating random otp ", random_opt
        
    end function email_t_gen_random_opt
    
    subroutine email_t_save_opt_cache(self, otp)
    class(email_t), intent(inout) :: self
    character(*), intent(inout) :: otp
    
    print *, "EMAIL: saving otp: ", otp, " to cache"
        
    end subroutine email_t_save_opt_cache
    
    function email_t_get_message(self, otp) result(msg)
    class(email_t), intent(inout) :: self
    character(*), intent(inout) :: otp
    character(:), allocatable :: msg
    
    msg = "EMAIL OTP for login is " // otp
        
    end function email_t_get_message
    
    subroutine email_t_send_notification(self, msg)
    class(email_t), intent(inout) :: self
    character(*), intent(inout) :: msg
    
    print *, "EMAIL: sending email: " // msg
        
    end subroutine email_t_send_notification
    
    subroutine email_t_publish_metric(self)
    class(email_t), intent(inout) :: self
    
    print *, "EMAIL: publishing metric"
        
    end subroutine email_t_publish_metric

end module template_method_module