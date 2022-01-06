module Types where

data Variable = Variable { name :: String, vtype :: String, value :: Int }
                deriving Show

type Env = [Variable]

