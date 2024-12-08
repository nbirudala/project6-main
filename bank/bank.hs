module Bank where

import Control.Monad.State

-- Define the BankOp as a state monad where the state is the current balance
type BankOp a = State Float a

-- Deposit an amount into the account
deposit :: Float -> BankOp ()
deposit amount = modify (+ amount)

-- Withdraw an amount from the account, allowing an overdraft of up to $100
withdraw :: Float -> BankOp Float
withdraw amount = do
    balance <- get
    let actualWithdraw = if balance - amount < -100
                         then balance + 100 -- Maximum possible overdraft
                         else amount
    put (balance - actualWithdraw)
    return actualWithdraw

-- Get the current balance of the account
getBalance :: BankOp Float
getBalance = get

-- Run the BankOp and return the final result
runBankOp :: BankOp a -> a
runBankOp op = evalState op 0.0 -- Start with an initial balance of $0.0
