module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Data.IORef

data BankAccount = BankAccount (IORef (Maybe Integer))  

closeAccount :: BankAccount -> IO ()
closeAccount (BankAccount ref) = atomicWriteIORef ref Nothing 

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance (BankAccount ref) = readIORef ref

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance (BankAccount ref) amount = atomicModifyIORef ref inc
    where inc v = let n = (+amount) <$> v
                  in (n,n)
    
openAccount :: IO BankAccount
openAccount = BankAccount <$> newIORef (Just 0) 
