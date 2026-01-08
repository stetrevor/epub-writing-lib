{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Styles where

import Base (Base(..))

class Styles repr where
  bodyStyle :: repr -> repr
  titleStyle :: repr -> repr
  subtitleStyle :: repr -> repr
  headingStyle :: repr -> repr
  paragraphStyle :: repr -> repr
  paragraphIndent :: repr -> repr
  dropcapStyle :: repr -> repr
  preStyle :: repr -> repr
  emphasisStyle :: repr -> repr
  strongStyle :: repr -> repr
  boxStyle :: repr -> repr
  itemizeStyle :: repr -> repr
  enumerateStyle :: repr -> repr
  itemStyle :: repr -> repr

class Attributes repr where
  fontFamily :: String -> repr
  fontSize :: String -> repr
  fontStyle :: String -> repr
  fontWeight :: String -> repr
  color :: String -> repr
  backgroundColor :: String -> repr
  margin :: String -> String -> String -> String -> repr
  padding :: String -> String -> String -> String -> repr
  indent :: String -> repr
  border :: String -> repr
  floatLeft :: repr

instance Styles [String] where
  bodyStyle s = ["body {" ++ concat s ++ "}"]
  titleStyle s =[ "h1 {" ++ concat s ++ "}"]
  subtitleStyle s = [".subtitle {" ++ concat s ++ "}"]
  headingStyle s = ["h2 {" ++ concat s ++ "}"]
  paragraphStyle s = ["p {" ++ concat s ++ "}"]
  paragraphIndent s = ["p + p {" ++ concat s ++ "}"]
  dropcapStyle s = [".dropcap {" ++ concat s ++ "}"]
  preStyle s = ["pre {" ++ concat s ++ "}"]
  emphasisStyle s = ["em {" ++ concat s ++ "}"]
  strongStyle s = ["strong {" ++ concat s ++ "}"]
  boxStyle s = [".box {" ++ concat s ++ "}"]
  itemizeStyle s = ["ul {" ++ concat s ++ "}"]
  enumerateStyle s = ["ol {" ++ concat s ++ "}"]
  itemStyle s = ["li {" ++ concat s ++ "}"]

instance Attributes [String] where
  fontFamily s = ["font-family: " ++ s ++ "; "]
  fontSize s = ["font-size: " ++ s ++ "; "]
  fontStyle s = ["font-style: " ++ s ++ "; "]
  fontWeight s = ["font-weight: " ++ s ++ "; "]
  color c = ["color: " ++ c ++ "; "]
  backgroundColor c = ["background-color: " ++ c ++ "; "]
  margin t r b l = ["margin: " ++ t ++ " " ++ r  ++ " " ++ b  ++ " " ++ l ++ "; "]
  padding t r b l = ["padding: " ++ t ++ " " ++ r  ++ " " ++ b  ++ " " ++ l ++ "; "]
  indent i = ["text-indent: " ++ i ++ "px; "]
  border s = ["border: " ++ s ++ "; "]
  floatLeft = ["float: left;"]

styleList :: (Styles repr, Attributes repr, Base repr) => [repr] -> repr
styleList = foldr1 (#)
