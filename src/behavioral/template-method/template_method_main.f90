program template_method_main

    use template_method_module, only: otp_type, sms_type, email_type

    type(otp_type) :: otp
    type(sms_type), target :: sms_otp
    type(email_type), target :: email_otp

    sms_otp = sms_type()
    otp%iopt => sms_otp
    call otp%gen_and_send_otp(4)

    write (*, *)

    email_otp = email_type()
    otp%iopt => email_otp
    call otp%gen_and_send_otp(4)

end program template_method_main

!> Results shall be:

!  SMS: generating random otp 1234
!  SMS: saving otp: 1234 to cache
!  SMS: sending sms: SMS OTP for login is 1234
!  SMS: publishing metric
!
!  EMAIL: generating random otp 1234
!  EMAIL: saving otp: 1234 to cache
!  EMAIL: sending email: EMAIL OTP for login is 1234
!  EMAIL: publishing metric