module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import LeibnizProof (type (~), symm, coerce, refl)

data Test a
  = I Int (a ~ Int)
  | B Boolean (a ~ Boolean)

int :: Int -> Test Int
int i = I i refl

bool :: Boolean -> Test Boolean
bool b = B b refl

eval :: forall a. Test a -> a
eval (I value proof) = coerce (symm proof) value
eval (B value proof) = coerce (symm proof) value

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  logShow $ eval $ int 5
  logShow $ eval $ bool true
