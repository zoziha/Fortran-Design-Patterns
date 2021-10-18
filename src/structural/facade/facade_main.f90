program facade_main
use facade_module, only: wallet_facade_t, new_wallet_facade
implicit none
type(wallet_facade_t) :: wallet_facade

    wallet_facade = new_wallet_facade(account_id="abc", code=1234)
    call wallet_facade%add_money_to_wallet(account_id="abc", security_code=1234, amount=10)
    call wallet_facade%deduct_money_from_wallet(account_id="abc", security_code=1234, amount=5)

end program facade_main

!> Results shall be:

!  Starting create account
!  Account created
!  Starting add money to wallet
!  Account Verified
!  SecurityCode Verified
!  Wallet balance added successfully
!  Sending wallet credit notification
!  Make ledger entry for accountId abc with txnType credit for amount           10
!  Starting debit money from wallet
!  Account Verified
!  SecurityCode Verified
!  Wallet balance added successfully
!  Sending wallet credit notification
!  Make ledger entry for accountId abc with txnType credit for amount            5