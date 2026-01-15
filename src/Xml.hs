{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Xml where

import Data.Text as T

class Xml repr where
  doc :: repr Element -> repr Element
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
  | T.null c = "<" <> n <> nvs' <> "/>"
  | T.null n = c
  | otherwise = "<" <> n <> nvs' <> ">" <> c <> "</" <> n <> ">"
  where
    nvs' = if T.null nvs then "" else " " <> nvs

instance Xml S where
  doc a = S ("", "", "<?xml version=\"1.0\"?>" <> view a)
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

newtype Dup repr repr' a = Dup { unDup :: (repr a, repr' a) }

duplicateXml :: (Xml repr, Xml repr') => (repr a, repr' a) -> (repr a, repr' a)
duplicateXml = id

instance (Xml repr, Xml repr') => Xml (Dup repr repr') where  
  doc (Dup (a, b)) = Dup (doc a, doc b)
  Dup (a1, a2) # Dup (b1, b2) = Dup (a1 # b1, a2 # b2)
  Dup (a1, a2) .> Dup (b1, b2) = Dup (a1 .> b1, a2 .> b2)
  Dup (a1, a2) .@ Dup (b1, b2) = Dup (a1 .@ b1, a2 .@ b2)
  Dup (a1, a2) .= Dup (b1, b2) = Dup (a1 .= b1, a2 .= b2)
  element s = Dup (element s, element s)
  attribute s = Dup (attribute s, attribute s)
  value s = Dup (value s, value s)
  string s = Dup (string s, string s)
  empty = Dup (Xml.empty, Xml.empty)

consumeXml :: (Xml repr, Xml repr') => (repr a1, repr' a1) -> (repr a1 -> a2) -> (repr' a1 -> b) -> (a2, b)
consumeXml e f g = (f e1, g e2)
  where
    (e1, e2) = duplicateXml e

attrs :: Xml repr => [(Text {- attribute -}, Text {- value -})] -> repr Attribution
attrs = Prelude.foldr (#) Xml.empty . Prelude.map (\(a, v) -> attribute a .= value v)
