module proxy_module

    use, intrinsic :: iso_fortran_env, only: int16
    implicit none
    private
    
    public :: nginx_t, new_nginx_server
    
    type, abstract :: server_t
    contains
        procedure(server_t_handle_request), deferred :: handle_request
    end type server_t
    
    abstract interface
        subroutine server_t_handle_request(self, url, method, code, msg)
            import server_t, int16
            class(server_t), intent(inout) :: self
            character(*), intent(in) :: url, method
            integer(int16), intent(out) :: code
            character(:), intent(out), allocatable :: msg 
        end subroutine server_t_handle_request
    end interface
    
    type map_t
        character(:), allocatable :: url
        integer(int16) :: rate_limiter
    end type map_t
    
    type, extends(server_t) :: nginx_t
        type(application_t), allocatable :: application
        integer(int16) :: max_allowed_request
        type(map_t), allocatable :: map(:)
        ! TODO:
    contains
        procedure :: handle_request      => nginx_t_handle_request
        procedure :: check_rate_limiting => nginx_t_check_rate_limiting
    end type nginx_t
    
    type, extends(server_t) :: application_t
    contains
        procedure :: handle_request => application_t_handle_request
    end type application_t
    
contains

    type(nginx_t) function new_nginx_server() result(nginx)
        type(map_t), allocatable :: map_(:)
        ! TODO:
        allocate(map_(2))
        map_(1) = map_t(url="/app/status", rate_limiter=0_int16)
        map_(2) = map_t(url="/create/user", rate_limiter=0_int16)
        
        nginx = nginx_t(application=application_t(), max_allowed_request=2, map=map_) ! TODO:
    end function new_nginx_server
    
    subroutine nginx_t_handle_request(self, url, method, code, msg)
        class(nginx_t), intent(inout) :: self
        character(*), intent(in) :: url, method
        integer(int16), intent(out) :: code
        character(:), intent(out), allocatable :: msg
        
        logical :: allowed
        
        allowed = self%check_rate_limiting(url)
        
        if (.not.allowed) then
            code = 403_int16
            msg  = "Not Allowed"
            return
        end if
        
        call self%application%handle_request(url, method, code, msg)
        
    end subroutine nginx_t_handle_request
    
    logical function nginx_t_check_rate_limiting(self, url) result(allowed)
        class(nginx_t), intent(inout) :: self
        character(*), intent(in) :: url
        
        integer(int16) :: i
        
        do i = 1_int16, size(self%map, kind=int16)
            if (self%map(i)%url == url) exit
        end do
        
        ! i = i - 1_int16
        
        if (self%map(i)%rate_limiter == 0_int16) then
            self%map(i)%rate_limiter = 1_int16
        end if
        
        if (self%map(i)%rate_limiter > self%max_allowed_request) then
            allowed = .false.
            return
        end if
        
        allowed = .true.
        self%map(i)%rate_limiter = self%map(i)%rate_limiter + 1_int16
        
    end function nginx_t_check_rate_limiting

    subroutine application_t_handle_request(self, url, method, code, msg)
        class(application_t), intent(inout) :: self
        character(*), intent(in) :: url, method
        integer(int16), intent(out) :: code
        character(:), intent(out), allocatable :: msg
        
        if (url=="/app/status" .and. method=="GET") then
            code = 200_int16
            msg = "Ok"
            return
        end if
        
        if (url=="/create/user" .and. method=="POST") then
            code = 201_int16
            msg = "User Created"
            return
        end if
        
        code = 404_int16
        msg = "Not Ok"
        
    end subroutine application_t_handle_request

end module proxy_module