!> Reference: https://refactoring.guru/design-patterns/observer/go/example
program test_observer

    use observer_pattern, only: item_type, customer_type, new_item
    type(item_type) :: shirt_item
    type(customer_type) :: observer_first, observer_second, observer_third

    !> A shirt item
    shirt_item = new_item("A Shirt")

    !> Some customers
    observer_first = customer_type(ID="abc@gmail.com")
    observer_second = customer_type(ID="def@gmail.com")
    observer_third = customer_type(ID="xyz@foxmail.com")

    !> Scene 1
    call shirt_item%register(observer_first)
    call shirt_item%register(observer_second)
    call shirt_item%update_availability()

    !> Scene 2
    call shirt_item%deregister(observer_first)
    call shirt_item%register(observer_third)
    call shirt_item%update_availability()

end program test_observer

!> Results shall be:

!  > Item A Shirt 👔 is now in stock.
!  Sending email to customer abc@gmail.com 📨 for item A Shirt.
!  Sending email to customer def@gmail.com 📨 for item A Shirt.
!  > Item A Shirt 👔 is now in stock.
!  Sending email to customer def@gmail.com 📨 for item A Shirt.
!  Sending email to customer xyz@foxmail.com 📨 for item A Shirt.