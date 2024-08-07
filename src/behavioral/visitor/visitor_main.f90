!> Reference: https://refactoring.guru/design-patterns/visitor/go/example
program test_visitor

    use visitor_pattern, only: square_type, circle_type, rectangle_type, area_calculator_type, middle_coordinates_type

    type(square_type) :: s = square_type(side=2)
    type(circle_type) :: c = circle_type(radius=3)
    type(rectangle_type) :: r = rectangle_type(l=2, b=3)

    type(area_calculator_type) :: a
    type(middle_coordinates_type) :: m

    !> area_calculator visiting shapes
    call s%accept(a)
    call c%accept(a)
    call r%accept(a)

    !> middle_coordinates visiting shapes
    call s%accept(m)
    call c%accept(m)
    call r%accept(m)

    !> Getting type of shape
    print *, s%get_type()
    print *, c%get_type()
    print *, r%get_type()

end program test_visitor

!> Results shall be:

!  Calculating area for square.🔥
!  Calculating area for circle.🔥
!  Calculating area for rectangle.🔥
!  Calculating middle point coordinates for square.💠
!  Calculating middle point coordinates for circle.💠
!  Calculating middle point coordinates for rectangle.💠
!  Square
!  Circle
!  Rectangle