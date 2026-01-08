{-# LANGUAGE LambdaCase #-}
module DryRun where
import Base (Base (..))
import Epub (Epub (..))
import Doc (Doc (..))
import Styles (Styles (..), Attributes (..))
import Prelude hiding (div)

data DryRun = DryRun { files :: [(String {- name -}, String {- content -})], acc :: String, epubF :: (String {- name -}, String {- directory -}) } deriving Show

emptyDR :: DryRun
emptyDR = DryRun { files=[], acc="", epubF=("", "") }

updateAcc :: (DryRun -> String) -> DryRun -> DryRun
updateAcc f dr = dr { acc = f dr }

fromAcc :: String -> DryRun
fromAcc s = emptyDR { acc=s }

fromFile :: String -> String -> DryRun
fromFile n c = emptyDR { files=[(n, c)] }

dryRun :: DryRun -> DryRun
dryRun = id

instance Base DryRun where
  (DryRun a1 a2 (a3, a4)) # (DryRun b1 b2 (b3, b4)) = DryRun (a1 ++ b1) (a2 ++ b2) (a3 ++ b3, a4 ++ b4)

instance Epub DryRun where
  epub d dr = dr { acc="", epubF=(d ++ ".epub", d) }
  containerxml = fromFile "META-INF/container.xml" "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<container version=\"1.0\" xmlns=\"urn:oasis:names:tc:opendocument:xmlns:container\">\n  <rootfiles>\n    <rootfile full-path=\"OEBPS/content.opf\" media-type=\"application/oebps-package+xml\"/>\n  </rootfiles>\n</container>"
  nav = updateAcc (\x -> "<nav epub:type=\"toc\">\n" ++ acc x ++ "</nav>\n")
  opf md mf sp = emptyDR { files=files md ++ files mf ++ files sp ++ [("OEBPS/content.opf", c)], acc=c }
    where
      c = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++
             "<package xmlns=\"http://www.idpf.org/2007/opf\" version=\"3.0\" unique-identifier=\"bookid\">\n" ++
             acc md ++
             acc mf ++
             acc sp ++
             "</package>\n"
  metadata = updateAcc $ \x -> "<metadata xmlns:opf=\"http://www.idpf.org/2007/opf\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\">\n" ++ acc x ++ "</metadata>\n" 
  title s = fromAcc $ "  <dc:title>" ++ s ++ "</dc:title>\n"
  language l = fromAcc $ "  <dc:language>" ++ l ++ "</dc:language>\n"
  identifier i = fromAcc $ "  <dc:identifier id=\"bookid\">" ++ i ++ "</dc:identifier>\n"
  modified m = fromAcc $ "  <meta property=\"dcterms:modified\">" ++ m ++ "</meta>\n"
  creator s = fromAcc $ "  <dc:creator>" ++ s ++ "</dc:creator>\n"
  manifest = updateAcc (\x -> "<manifest>\n" ++ acc x ++ "</manifest>\n")
  mitem i t = updateAcc (\x -> "  <item id=\"" ++ i ++ "\" media-type=\"" ++ t ++ "\" href=\"" ++ acc x ++ "\"/>\n") -- filename is passed in through `acc`
  toc = updateAcc (\x -> "  <item id=\"toc\" media-type=\"application/xhtml+xml\"  properties=\"nav\" " ++ "href=\"" ++ acc x ++ "\"/>\n") 
  spine = updateAcc (\x -> "<spine>\n" ++ acc x ++ "</spine>\n")
  idref s = fromAcc $ "  <itemref idref=\"" ++ s ++ "\"/>\n"
  xhtml n t dr = dr { files=files dr ++ [("OEBPS/" ++ n, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<html xmlns=\"http://www.w3.org/1999/xhtml\" xmlns:epub=\"http://www.idpf.org/2007/ops\">\n<head>\n  <meta charset=\"UTF-8\" />\n  <title>" ++ t ++ "</title>\n  <link rel=\"stylesheet\" type=\"text/css\" href=\"stylesheet.css\" />\n</head>\n<body>\n" ++ acc dr ++ "</body>\n</html>\n")], acc=n } -- pass filename
  stylesheet dr = dr { files=files dr ++ [("OEBPS/stylesheet.css", acc dr)], acc="stylesheet.css" } -- pass filename

tag :: String -> String -> String
tag t s = "<" ++ t ++ ">" ++ s ++ "</" ++ t ++ ">\n"

div :: String -> String -> String
div cls s = "<div class=\"" ++ cls ++ "\">" ++ s ++ "</div>\n"

instance Doc DryRun where
  empty = fromAcc ""
  enumerate = updateAcc $ tag "ol" . acc
  item = updateAcc $ tag "li" . acc
  link h c = fromAcc $ "<a href=\"" ++ h ++ "\">" ++ c ++ "</a>\n"
  title = fromAcc . tag "h1"
  subtitle = fromAcc . div "subtitle"
  heading = fromAcc . tag "h2"
  subheading = fromAcc . tag "h3"
  paragraph = updateAcc $ tag "p" . acc
  dropcap = updateAcc $ div "dropcap" . acc
  text = fromAcc
  pre = fromAcc . tag "pre"
  emphasis = updateAcc $ tag "em" . acc
  strong = updateAcc $ tag "strong" . acc
  box = updateAcc $ div "box" . acc

curly :: String -> String -> String
curly p s = p ++ " {" ++ s ++ "}\n"

instance Styles DryRun where
  bodyStyle = updateAcc $ curly "body" . acc
  titleStyle = updateAcc $ curly "h1" . acc
  subtitleStyle = updateAcc $ curly ".subtitle" . acc
  headingStyle = updateAcc $ curly "h2" . acc
  paragraphStyle = updateAcc $ curly "p" . acc
  paragraphIndent = updateAcc $ curly "p + p" . acc
  dropcapStyle = updateAcc $ curly ".dropcap" . acc
  preStyle = updateAcc $ curly "pre" . acc
  emphasisStyle = updateAcc $ curly "em" . acc
  strongStyle = updateAcc $ curly "strong" . acc
  boxStyle = updateAcc $ curly ".box" . acc
  itemizeStyle = updateAcc $ curly "ul" . acc
  enumerateStyle = updateAcc $ curly "ol" . acc
  itemStyle = updateAcc $ curly "li" . acc

attr :: String -> String -> String
attr n v = "\n  " ++ n ++ ": " ++ v ++ "; \n"

instance Attributes DryRun where
  fontFamily s = fromAcc $ attr "font-family" s
  fontSize s = fromAcc $ attr "font-size" s
  fontStyle s = fromAcc $ attr "font-style" s
  fontWeight s = fromAcc $ attr "font-weight" s
  color s = fromAcc $ attr "color" s
  backgroundColor s = fromAcc $ attr "background-color" s
  margin t r b l = fromAcc $ attr "margin: " $ t ++ " " ++ r  ++ " " ++ b  ++ " " ++ l
  padding t r b l = fromAcc $ attr "padding: " $ t ++ " " ++ r  ++ " " ++ b  ++ " " ++ l
  indent s = fromAcc $ attr "text-indent" s
  border s = fromAcc $ attr "border" s
  floatLeft = fromAcc "float: left;"
