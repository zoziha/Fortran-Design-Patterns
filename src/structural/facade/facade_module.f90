module facade_module

    implicit none
    private

    public :: new_wallet_facade, wallet_facade_t

    type account_t
        character(:), allocatable :: name
    contains
        procedure :: check_account => account_t_check_account
    end type account_t

    type security_code_t
        integer :: code
    contains
        procedure :: check_code => security_code_t_check_code
    end type security_code_t

    type wallet_t
        integer :: balance
    contains
        procedure :: credit_balance => wallet_t_credit_balance
        procedure :: debit_balance => wallet_t_debit_balance
    end type wallet_t

    type ledger_t
    contains
        procedure :: make_entry => ledger_t_make_entry
    end type ledger_t

    type notification_t
    contains
        procedure :: send_wallet_credit_notification => notification_t_send_wallet_credit_notification
        procedure :: send_wallet_debit_notification => notification_t_send_wallet_debit_notification
    end type notification_t

    type wallet_facade_t
        type(account_t) :: account
        type(wallet_t) :: wallet
        type(security_code_t) :: security_code
        type(notification_t) :: notification
        type(ledger_t) :: ledger
    contains
        procedure :: add_money_to_wallet => wallet_facade_t_add_money_to_wallet
        procedure :: deduct_money_from_wallet => wallet_facade_t_deduct_money_from_wallet
    end type wallet_facade_t

contains

    function new_wallet_facade(account_id, code) result(wallet_facade)
        character(*), intent(in) :: account_id
        integer, intent(in) :: code
        type(wallet_facade_t) :: wallet_facade
        print *, "Starting create account"
        wallet_facade = wallet_facade_t(account=account_t(account_id), &
                                        security_code=security_code_t(code), &
                                        wallet=wallet_t(balance=0), &
                                        notification=notification_t(), &
                                        ledger=ledger_t())
        print *, "Account created"
    end function new_wallet_facade

    subroutine wallet_facade_t_add_money_to_wallet(self, account_id, security_code, amount)
        class(wallet_facade_t), intent(inout) :: self
        character(*), intent(in) :: account_id
        integer, intent(in) :: security_code, amount
        print *, "Starting add money to wallet"
        call self%account%check_account(account_id)
        call self%security_code%check_code(security_code)
        call self%wallet%credit_balance(amount)
        call self%notification%send_wallet_credit_notification()
        call self%ledger%make_entry(account_id, "credit", amount)
    end subroutine wallet_facade_t_add_money_to_wallet

    subroutine wallet_facade_t_deduct_money_from_wallet(self, account_id, security_code, amount)
        class(wallet_facade_t), intent(inout) :: self
        character(*), intent(in) :: account_id
        integer, intent(in) :: security_code, amount
        print *, "Starting debit money from wallet"
        call self%account%check_account(account_id)
        call self%security_code%check_code(security_code)
        call self%wallet%credit_balance(amount)
        call self%notification%send_wallet_credit_notification()
        call self%ledger%make_entry(account_id, "credit", amount)
    end subroutine wallet_facade_t_deduct_money_from_wallet

    ! - - - - - - - - -

    subroutine account_t_check_account(self, account_name)
        class(account_t), intent(inout) :: self
        character(*), intent(in) :: account_name
        if (self%name /= account_name) then
            error stop "Account Name is incorrect"
        end if
        print *, "Account Verified"
    end subroutine account_t_check_account

    ! - - - - - - - - -

    subroutine security_code_t_check_code(self, incomming_code)
        class(security_code_t), intent(inout) :: self
        integer, intent(in) :: incomming_code
        if (self%code /= incomming_code) then
            error stop "Security Code is incorrect"
        end if
        print *, "SecurityCode Verified"
    end subroutine security_code_t_check_code

    ! - - - - - - - - -

    subroutine wallet_t_credit_balance(self, amount)
        class(wallet_t), intent(inout) :: self
        integer, intent(in) :: amount
        self%balance = self%balance + amount
        print *, "Wallet balance added successfully"
    end subroutine wallet_t_credit_balance

    subroutine wallet_t_debit_balance(self, amount)
        class(wallet_t), intent(inout) :: self
        integer, intent(in) :: amount
        if (self%balance < amount) then
            error stop "Balance is not sufficient"
        end if
        print *, "Wallet balance is Sufficient"
        self%balance = self%balance - amount
    end subroutine wallet_t_debit_balance

    ! - - - - - - - - -

    subroutine ledger_t_make_entry(self, account_id, txn_type, amount)
        class(ledger_t), intent(inout) :: self
        character(*), intent(in) :: account_id, txn_type
        integer, intent(in) :: amount
        print *, "Make ledger entry for accountId ", account_id, &
            " with txnType ", txn_type, &
            " for amount ", amount
    end subroutine ledger_t_make_entry

    ! - - - - - - - - -

    subroutine notification_t_send_wallet_credit_notification(self)
        class(notification_t), intent(inout) :: self
        print *, "Sending wallet credit notification"
    end subroutine notification_t_send_wallet_credit_notification

    subroutine notification_t_send_wallet_debit_notification(self)
        class(notification_t), intent(inout) :: self
        print *, "Sending wallet debit notification"
    end subroutine notification_t_send_wallet_debit_notification

end module facade_module
