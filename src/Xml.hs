{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Xml where

import Data.Text as T ( null, Text )

class Xml repr where
  dcl :: repr Element -> repr Element
  (#) :: repr a -> repr a -> repr a
  (.>) :: repr Element {- parent -} -> repr Element {- child -} -> repr Element
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

infixr 5 #
infixr 6 .>
infixr 7 .@
infixr 8 .=

newtype S a = S (Text {- name -}, Text {- attribution -}, Text {- children -}) deriving Show

view :: S a -> Text
view (S (n, nvs, c)) 
  | T.null n = c
  | T.null c = "<" <> n <> nvs' <> "/>"
  | otherwise = "<" <> n <> nvs' <> ">" <> c <> "</" <> n <> ">"
  where
    nvs' = if T.null nvs then "" else " " <> nvs

instance Xml S where
  dcl a = S ("", "", "<?xml version=\"1.0\"?>" <> view a)
  a@(S (_, a2, _)) # b@(S (_, b2, _)) = S ("", nvs a2 b2, view a <> view b)
    where
      nvs :: Text -> Text -> Text
      nvs "" y = y
      nvs x "" = x
      nvs x y = x <> " " <> y
  S (a1, a2, _) .> b = S (a1, a2, view b)
  S (a1, _, a3) .@ S (_, b2, _) = S (a1, b2, a3)
  S (_, _, a) .= S (_, _, b) = S ("", a <> "=\"" <> b <> "\"", "")
  element n = S (n, "", "")
  attribute = S . ("", "", )
  value = S . ("", "", )
  string = S . ("", "", )
  empty = S ("", "", "")

attrs :: Xml repr => [(Text {- attribute -}, Text {- value -})] -> repr Attribution
attrs = Prelude.foldr (#) Xml.empty . Prelude.map (\(a, v) -> attribute a .= value v)
