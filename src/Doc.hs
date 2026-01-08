{-# LANGUAGE FlexibleInstances #-}
module Doc where
import Base (Base (..))

class Doc repr where
  empty :: repr
  enumerate :: repr -> repr
  item :: repr -> repr
  link :: String {- href -} -> String {- label -} -> repr
  title :: String -> repr
  subtitle :: String -> repr
  heading :: String -> repr
  subheading :: String -> repr
  paragraph :: repr -> repr
  dropcap :: repr -> repr
  text :: String -> repr
  pre :: String -> repr
  emphasis :: repr -> repr
  strong :: repr -> repr
  box :: repr -> repr

section' :: (Doc repr, Base repr) => [repr] -> repr
section' = foldr1 (#)

p :: Doc repr => String -> repr
p = paragraph . text

p2 :: Doc repr => repr -> repr
p2 = paragraph

italic :: Doc repr => String -> repr
italic = emphasis . text

em :: Doc repr => String -> repr
em = italic

bold :: Doc repr => String -> repr
bold = strong . text

b :: Doc repr => String -> repr
b = bold
