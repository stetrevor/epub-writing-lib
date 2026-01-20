{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Xml where

import Data.Text as T

class Xml repr where
  dcl :: repr Element -> repr Element
  (.#) :: repr Element -> repr Element -> repr Element
  (.>) :: repr Element {- parent -} -> repr Element {- child -} -> repr Element
  (.+) :: repr Attribution -> repr Attribution -> repr Attribution
  (.@) :: repr Element -> repr Attribution -> repr Element
  (.=) :: repr Attribute -> repr Value -> repr Attribution
  element :: Text {- name -} -> repr Element
  attribute :: Text {- name -} -> repr Attribute
  value :: Text {- value -} -> repr Value
  string :: Text -> repr Element
  empty :: repr a

data Element
data Attribution
data Attribute
data Value

infixr 4 .#
infixr 5 .>
infixr 6 .@
infixr 7 .+
infixr 8 .=

(#) :: Xml repr => repr Element -> repr Element -> repr Element
(#) = (.#)

infixr 4 #

data S a = E Text Text Text | AV Text | A Text | V Text | T Text | Z deriving Show

view :: S a -> Text
view = \case
  E n av c | isEmptyElement n -> "<" <> n <> (if T.null av then "" else " " <> av) <> "/>"
           | otherwise -> "<" <> n <> (if T.null av then "" else " " <> av) <> ">" <> c <> "</" <> n <> ">"
  AV t -> t
  A t -> t
  V t -> t
  T t -> t
  Z -> ""

isEmptyElement :: Text -> Bool
isEmptyElement "area" = True
isEmptyElement "base" = True
isEmptyElement "br" = True
isEmptyElement "col" = True
isEmptyElement "embed" = True
isEmptyElement "hr" = True
isEmptyElement "img" = True
isEmptyElement "input" = True
isEmptyElement "keygen" = True
isEmptyElement "link" = True
isEmptyElement "meta" = True
isEmptyElement "param" = True
isEmptyElement "source" = True
isEmptyElement "track" = True
isEmptyElement "wbr" = True
isEmptyElement _ = False

instance Xml S where
  dcl a = T ("<?xml version=\"1.0\"?>" <> view a)
  a .# b = T $ view a <> view b
  E n av _ .> b = E n av (view b)
  _ .> _ = Z
  AV a .+ AV b = AV (a <> " " <> b)
  Z .+ a = a
  a .+ Z = a
  _ .+ _ = Z
  E n _ c.@ AV av = E n av c
  a .@ Z = a
  _ .@ _ = Z
  A a .= V v = AV (a <> "=\"" <> v <> "\"")
  A a .= Z = AV a
  _ .= _ = AV "4"
  element n = E n "" ""
  attribute = A
  value = V
  string = T
  empty = Z

attrs :: Xml repr => [(Text {- attribute -}, Text {- value -})] -> repr Attribution
attrs = Prelude.foldr (.+) Xml.empty . Prelude.map (\(a, v) -> attribute a .= value v)
