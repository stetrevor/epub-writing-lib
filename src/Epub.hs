{-# LANGUAGE FlexibleInstances #-}
module Epub where

class Epub repr where
  epub :: String -> repr -> repr
  containerxml :: repr
  nav :: repr -> repr
  opf :: repr -> repr -> repr -> repr
  metadata :: repr -> repr
  title :: String -> repr
  language :: String -> repr
  identifier :: String -> repr
  modified :: String -> repr
  creator :: String -> repr
  manifest :: repr -> repr
  mitem :: String {- id -} -> String {- media type -} -> repr -> repr
  toc :: repr -> repr
  spine :: repr -> repr
  idref :: String -> repr
  xhtml :: String {- name -} -> String {- title -} -> repr -> repr
  stylesheet :: repr -> repr
