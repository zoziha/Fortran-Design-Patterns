program proxy_main

    use, intrinsic :: iso_fortran_env, only: int16
    use proxy_module, only: nginx_t, new_nginx_server

    type(nginx_t) :: nginx_server
    character(*), parameter :: app_status_url = "/app/status", create_user_url = "/create/user"
    integer(int16) :: code
    character(:), allocatable :: body

    nginx_server = new_nginx_server()

    call nginx_server%handle_request(app_status_url, "GET", code, body)
    print *, "Url: ", app_status_url, new_line(""), &
        "Http code: ", code, new_line(""), &
        "Body: ", body

    call nginx_server%handle_request(app_status_url, "GET", code, body)
    print *, "Url: ", app_status_url, new_line(""), &
        "Http code: ", code, new_line(""), &
        "Body: ", body

    call nginx_server%handle_request(app_status_url, "GET", code, body)
    print *, "Url: ", app_status_url, new_line(""), &
        "Http code: ", code, new_line(""), &
        "Body: ", body

    call nginx_server%handle_request(create_user_url, "POST", code, body)
    print *, "Url: ", create_user_url, new_line(""), &
        "Http code: ", code, new_line(""), &
        "Body: ", body

    call nginx_server%handle_request(create_user_url, "GET", code, body)
    print *, "Url: ", create_user_url, new_line(""), &
        "Http code: ", code, new_line(""), &
        "Body: ", body

end program proxy_main

!> Results shall be:

!  Url: /app/status
!  Http code:     200
!  Body: Ok
!  Url: /app/status
!  Http code:     200
!  Body: Ok
!  Url: /app/status
!  Http code:     403
!  Body: Not Allowed
!  Url: /create/user
!  Http code:     201
!  Body: User Created
!  Url: /create/user
!  Http code:     404
!  Body: Not Ok