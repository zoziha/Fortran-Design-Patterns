module facade_module

    implicit none
    private

    public :: new_wallet_facade, wallet_facade_type

    type account_type
        character(:), allocatable :: name
    contains
        procedure :: check_account => account_type_check_account
    end type account_type

    type security_code_type
        integer :: code
    contains
        procedure :: check_code => security_code_type_check_code
    end type security_code_type

    type wallet_type
        integer :: balance
    contains
        procedure :: credit_balance => wallet_type_credit_balance
        procedure :: debit_balance => wallet_type_debit_balance
    end type wallet_type

    type ledger_type
    contains
        procedure :: make_entry => ledger_type_make_entry
    end type ledger_type

    type notification_type
    contains
        procedure :: send_wallet_credit_notification => notification_type_send_wallet_credit_notification
        procedure :: send_wallet_debit_notification => notification_type_send_wallet_debit_notification
    end type notification_type

    type wallet_facade_type
        type(account_type) :: account
        type(wallet_type) :: wallet
        type(security_code_type) :: security_code
        type(notification_type) :: notification
        type(ledger_type) :: ledger
    contains
        procedure :: add_money_to_wallet => wallet_facade_type_add_money_to_wallet
        procedure :: deduct_money_from_wallet => wallet_facade_type_deduct_money_from_wallet
    end type wallet_facade_type

contains

    function new_wallet_facade(account_id, code) result(wallet_facade)
        character(*), intent(in) :: account_id
        integer, intent(in) :: code
        type(wallet_facade_type) :: wallet_facade
        print *, "Starting create account"
        wallet_facade = wallet_facade_type(account=account_type(account_id), &
                                        security_code=security_code_type(code), &
                                        wallet=wallet_type(balance=0), &
                                        notification=notification_type(), &
                                        ledger=ledger_type())
        print *, "Account created"
    end function new_wallet_facade

    subroutine wallet_facade_type_add_money_to_wallet(self, account_id, security_code, amount)
        class(wallet_facade_type), intent(inout) :: self
        character(*), intent(in) :: account_id
        integer, intent(in) :: security_code, amount
        print *, "Starting add money to wallet"
        call self%account%check_account(account_id)
        call self%security_code%check_code(security_code)
        call self%wallet%credit_balance(amount)
        call self%notification%send_wallet_credit_notification()
        call self%ledger%make_entry(account_id, "credit", amount)
    end subroutine wallet_facade_type_add_money_to_wallet

    subroutine wallet_facade_type_deduct_money_from_wallet(self, account_id, security_code, amount)
        class(wallet_facade_type), intent(inout) :: self
        character(*), intent(in) :: account_id
        integer, intent(in) :: security_code, amount
        print *, "Starting debit money from wallet"
        call self%account%check_account(account_id)
        call self%security_code%check_code(security_code)
        call self%wallet%credit_balance(amount)
        call self%notification%send_wallet_credit_notification()
        call self%ledger%make_entry(account_id, "credit", amount)
    end subroutine wallet_facade_type_deduct_money_from_wallet

    ! - - - - - - - - -

    subroutine account_type_check_account(self, account_name)
        class(account_type), intent(inout) :: self
        character(*), intent(in) :: account_name
        if (self%name /= account_name) then
            error stop "Account Name is incorrect"
        end if
        print *, "Account Verified"
    end subroutine account_type_check_account

    ! - - - - - - - - -

    subroutine security_code_type_check_code(self, incomming_code)
        class(security_code_type), intent(inout) :: self
        integer, intent(in) :: incomming_code
        if (self%code /= incomming_code) then
            error stop "Security Code is incorrect"
        end if
        print *, "SecurityCode Verified"
    end subroutine security_code_type_check_code

    ! - - - - - - - - -

    subroutine wallet_type_credit_balance(self, amount)
        class(wallet_type), intent(inout) :: self
        integer, intent(in) :: amount
        self%balance = self%balance + amount
        print *, "Wallet balance added successfully"
    end subroutine wallet_type_credit_balance

    subroutine wallet_type_debit_balance(self, amount)
        class(wallet_type), intent(inout) :: self
        integer, intent(in) :: amount
        if (self%balance < amount) then
            error stop "Balance is not sufficient"
        end if
        print *, "Wallet balance is Sufficient"
        self%balance = self%balance - amount
    end subroutine wallet_type_debit_balance

    ! - - - - - - - - -

    subroutine ledger_type_make_entry(self, account_id, txn_type, amount)
        class(ledger_type), intent(inout) :: self
        character(*), intent(in) :: account_id, txn_type
        integer, intent(in) :: amount
        print *, "Make ledger entry for accountId ", account_id, &
            " with txnType ", txn_type, &
            " for amount ", amount
    end subroutine ledger_type_make_entry

    ! - - - - - - - - -

    subroutine notification_type_send_wallet_credit_notification(self)
        class(notification_type), intent(inout) :: self
        print *, "Sending wallet credit notification"
    end subroutine notification_type_send_wallet_credit_notification

    subroutine notification_type_send_wallet_debit_notification(self)
        class(notification_type), intent(inout) :: self
        print *, "Sending wallet debit notification"
    end subroutine notification_type_send_wallet_debit_notification

end module facade_module
