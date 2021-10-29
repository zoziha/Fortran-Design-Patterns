program template_method_main

    use template_method_module, only: otp_t, sms_t, email_t

    type(otp_t) :: otp
    type(sms_t), target :: sms_otp
    type(email_t), target :: email_otp

    sms_otp = sms_t()
    otp%iopt => sms_otp
    call otp%gen_and_send_otp(4)

    write (*, *)

    email_otp = email_t()
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