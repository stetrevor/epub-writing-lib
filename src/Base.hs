{-# LANGUAGE FlexibleInstances #-}
module Base where

class Base repr where
  (#) :: repr -> repr -> repr

infixr 5 #
