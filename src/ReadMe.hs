{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module ReadMe where
import Xml (Xml (..), Element, view, (#))
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Text (Text)

readme :: Xml repr => repr Element
readme = (
  h1 "How to Use"
  # p "This is what a complete XHTML file looks like:"
  # pre demo
  # p "You can define your own XHTML files using the same combinators."
  # p "This is everything you need to create an EPUB:"
  # pre demo2
  # p2 (s "First, you specify the output parameters, then you create the metadata list, then the static assets, and all the xhtml files, and the spine section of the package document. Finally you can call " # c "genEpub" # s " to generate the EPUB file.")
  # h1 "What is This Library and Why"
  # p "This is a library to aid in EPUB book writing. This is ideal if your epub is flowing layout or simple pages of pictures. It provides an embedded DSL(Domain Specific Language) for authoring XHTML file contents. It also provides functions to generate the package document and container file required for EPUB files."
  # p "Writing XHTML can get tedious very quickly. There are many markup languages to simplify the process. But as I used them before, I often found them falling short of what I wanted to express."
  # h2 "Markdown vs This Library "
  # p "Languages like markdown has limited set of features. Trying to add classes and id support are not easy."
  # p "This library provides a set of combinators to express XHTML structure. You can create elements, attributes, and values. You can nest elements and add attributes to them. This gives you the full power of XHTML without the verbosity of raw XML. Writing XML or XHTML is writing Haskell functions. "
  # h2 "DSL for XML"
  # pre demo3
  # p2 (s "The DSL uses final encoding. It’s inspired by " # em (ahs "https://hackage.haskell.org/package/type-of-html" "type-of-html") # s ". This DSL defines the syntax in this way: ")
  # p2 (c ".>" # s " is a combinator from a parent element to a child element. ")
  # p2 (c ".@" # s " connects an element with its attributions. ")
  # p2 (c ".=" # s " assigns a value to an attribute. ")
  # p2 (c "dcl" # s " is the the XML declaration line. Although its type allows it to be used anywhere in a document, its correct use is at the outmost of everything else. ")
  # p2 (c "element" # s " represents a tag. ")
  # p2 (c "attribute" # s " and " # c "value" # s " are self explanatory. ")
  # p2 (c "empty" # s " provides an escape hatch for when nothing needs to be represented but an Element, Attribution, Attribute or Value is still needed. ")
  # h2 "Initial Encoding vs Final Encoding"
  # p2 (s "In " # i "type-of-html" # s ", every tag name, attribute name or attribute value is defined and exactly typed. This has two issues when it comes to hand writing html.")
  # p "First is that the final type is very long and unwieldy. Although with IDE, getting the correct type is easier, but the type alone can still span multiple lines for a typical HTML document. And every time you make a change in anything, the type needs to be updated to reflect that change."
  # p "Second is there is no way to use any element or attribution from other namespaces other than HTML. This makes it impossible to author XHTML and XML at the same time. "
  # p2 (s "Our final encoding here has no such issues. The type of the document is " # c "xml repr => repr Element" # s " always. ")
  # p "And you are free to use any elements and attributions from any namespaces. "
  # h2 "The Real Power of the DSL"
  # p "Apart from the features mentioned above. Those are things you can do with the DSL:"
  # p "DNRIY: if you use any thing often, write a function for it, this is handy when you have predefined styles for things like titles and pictures. "
  # p "A template for a document is just another function. You can write it yourself. "
  # p "Another things you can do is element generation with state. You can generate sequential ids for anchors in a document: "
  # p "Another things you can do is element generation with state. You can generate sequential ids for anchors in a document: "
  # pre demo4
  # h2 "EPUB Output as a Bonus"
  # p "An EPUB is a collection of xml files, XHTML files and static assets like pdf, images, audios or videos. Th DSL can represent everything that&rsquo;s XML. The EPUB generation is just a thin wrapper to collect output parameters. It needs to know the metadata, the static assets it contains, the XHTML files it contains to generate the package document and put everything in correct folders. Then just zip everything according to EPUB specifications. "
  # h2 "Where is the mimetype File"
  # p2 (s "Although this is an implementation detail, the " # i "mimetype" # s " only exists directly in the archived epub file. It&rsquo;s not generated in the output folder. Simply zipping the output folder, even with the correct " # i "mimetype" # s " file present, will not produce a correct epub file. ")
  # h2 "EPUB Checker"
  # p2 (s "To verify everything is in order, the " # ahs "https://www.w3.org/publishing/epubcheck/" "epub checker" # s " from w3c.org is recommended. ")
  # h2 "Generated XHTML is Unreadable"
  # p2 (s "The generated XHTML files contains no indentation or new lines unnecessary. This makes no difference in the epub output. To read the generated files, a formatter from your editor is recommended. And using the " # c "zipF" # s " function provided, the any changes you make in the output folder will make it to the generated epub. ")

  # p "Why not formatting the files during generation? XHTML is after all whitespace sensitive. While it&rsquo;s possible to make an educated guess at what tags are not affected by whitespaces, and indent them accordingly, CSS still can alter the display of an element. It is beyond the scope of this project to produce readable XHTML files. Our goal is to produce readable EPUB, with the least amount of pain possible. "
  )
  where
    ts t txt = element t .> string txt
    i = ts "em"
    h1 = ts "h1"
    h2 = ts "h2"
    p = ts "p"
    pre = ts "pre"
    p2 = (element "p" .>)
    s = string
    c = (element "code" .>) . string
    em = (element "em" .>)
    ahs h x = element "a" .@ (attribute "href" .= value h) .> string x
    demo = T.unlines
      [ "chapter1 :: Xml repr => repr Element"
      , "chapter1 = dcl $ element \"html\" .@"
      , "    attribute \"xmlns\" .= value \"http://www.w3.org/1999/xhtml\""
      , "  .> ("
      , "      element \"head\" .> element \"title\" .> string \"Chapter 1\""
      , "      # element \"body\" .> element \"h1\" .> string \"Chapter 1 Title\""
      , "      # element \"p\" .> string \"This is the first chapter.\""
      , "      )"
      ]
    demo2 = T.unlines
      [ "output :: Output"
      , "output = Output { odir = \".output-epub\", oname = \"demo\", oassets = \"./app/assets\" }"
      , ""
      , "metadata :: [Metadata]"
      , "metadata = ["
      , "      Title \"Apple Books EPUB Example v3.3\""
      , "    , Identifier \"0123456789\""
      , "    , Lang \"en\""
      , "    , Modified \"2024-05-01T01:00:00Z\""
      , "    , Rights \"This book is copyrighted by Apple Inc and must not be shared.\""
      , "    , Creator \"Apple Inc.\""
      , "    , Version \"3.3\""
      , "    , SpecifiedFonts True"
      , "    ]"
      , ""
      , "assets :: [Asset]"
      , "assets = ["
      , "      audioA \"aud1\" \"audio/Hot_Air_Audio.m4a\""
      , "    , fontA \"font-DrukWide\" \"fonts/DrukWide-Medium.ttf\""
      , "    , pngA \"img3\" \"images/balloon_data.png\""
      , "    , jpgA \"frequency\" \"images/frequency.jpg\""
      , "    , mitem \"cover-image\" \"images/cover-image.png\" \"image/png\" \"cover-image\""
      , "    , pdfA \"pdf\" \"pdf/luftschiffahrt.pdf\""
      , "    , videoA \"vid1\" \"video/inflating-balloon.mp4\""
      , "    , cssA \"css-cht\" \"css/balloon_data.css\""
      , "    ]"
      , ""
      , "xhtmls :: [XhtmlFile]"
      , "xhtmls = ["
      , "      xhtmlA \"chart\" \"balloon_data.xhtml\" $ view balloon_dataXhtml"
      , "    , xhtmlA \"bib\" \"bibliography.xhtml\" $ view bibliographyXhtml"
      , "    , xhtmlA \"j2026_01_07\" \"2026_01_07.xhtml\" $ view journal_2026_01_07"
      , "    , xhtmlA' \"chapter1\" \"chapter1.xhtml\" \"svg\" $ view chapter1Xhtml"
      , "    , xhtmlA \"chapter2\" \"chapter2.xhtml\" $ view chapter2Xhtml"
      , "    , xhtmlA \"cover\" \"cover.xhtml\" $ view coverXhtml"
      , "    , xhtmlA \"end\" \"endnotes.xhtml\" $ view endnotesXhtml"
      , "    , xhtmlA' \"toc\" \"toc.xhtml\" \"nav\" $ view tocXhtml"
      , "    ]"
      , ""
      , "sps :: [SpineItem]"
      , "sps = [spi \"cover\" "
      , "    , spi \"chapter1\""
      , "    , spi \"chapter2\""
      , "    , spi \"j2026_01_07\""
      , "    , spi' \"end\""
      , "    , spi \"bib\""
      , "    , spi' \"chart\""
      , "    ]"
      , ""
      , "book :: IO ()"
      , "book = genEpub output metadata assets xhtmls sps"
      ]
    demo3 = T.unlines
      [ "class Xml repr where"
      , "  dcl :: repr Element -> repr Element"
      , "  (#) :: repr Element -> repr Element -> repr Element"
      , "  (.>) :: repr Element -> repr Element -> repr Element"
      , "  (.@) :: repr Element -> repr Attribution -> repr Element"
      , "  (.=) :: repr Attribute -> repr Value -> repr Attribution"
      , "  element :: Text {- name -} -> repr Element"
      , "  attribute :: Text {- name -} -> repr Attribute"
      , "  value :: Text {- value -} -> repr Value"
      , "  string :: Text -> repr Element"
      , "  empty :: repr a"
      ]
    demo4 = T.unlines
      ["newtype R repr a = R { unR :: State Int (repr a) }"
       , ""
       , "instance Xml repr => Xml (R repr) where"
       , "  dcl e = R $ unR e >>= return . dcl"
       , "  a # b = R $ liftA2 (#) (unR a) (unR b)"
       , "  a .> b = R $ liftA2 (.>) (unR a) (unR b)"
       , "  a .@ b = R $ liftA2 (.@) (unR a) (unR b)"
       , "  a .= b = R $ liftA2 (.=) (unR a) (unR b)"
       , "  element = R . return . element"
       , "  attribute = R . return . attribute"
       , "  value s"
       , "    | s == idmark = R $ do"
       , "      i <- get"
       , "      put (i + 1)"
       , "      return (value (anchor i))"
       , "    | otherwise = R $ return (value s)"
       , "  string = R . return . string"
       , "  empty = R (return Xml.empty)"
       , ""
       , "addAnchors :: R repr a -> repr a"
       , "addAnchors e = evalState (unR e) 1"
       , ""
       , "idmark :: Text"
       , "idmark = \"__GENERATE_ID__\""
       , ""
       , "anchor :: Int -> Text"
       , "anchor i = \"anchor-\" <> T.pack (show i)"
      ]

readme' :: Xml repr => repr Element
readme' = tpl "ReadMe" readme
  where
    tpl t e = dcl $ element "html" .@
      attribute "xmlns" .= value "http://www.w3.org/1999/xhtml"
      .> (
        element "head" .> element "title" .> string t
        # element "body" .> e
        )

genXhtml :: IO ()
genXhtml = T.writeFile "README.xhtml" (view readme')

genMd :: IO ()
genMd = T.writeFile "README.md" (md readme)

test :: Text
test = md $ element "a" .@ (attribute "href" .= value "https://www.w3.org/publishing/epubcheck/") .> string "epub checker"

data Md a = BE Block | IE Inline | T Text | Z

data Block = P Text | H1 Text | H2 Text | Pre Text

data Inline = Code Text | Em Text | Link Text Text | Href Text

md :: Md a -> Text
md (BE b) = case b of
  P t -> t <> "\n"
  H1 t -> "# " <> t <> "\n"
  H2 t -> "## " <> t <> "\n"
  Pre t -> "```\n" <> t <> "\n```\n"
md (IE i) = case i of
  Code t -> "`" <> t <> "`"
  Em t -> "*" <> t <> "*"
  Link h x -> "[" <> x <> "](" <> h <> ")"
  Href h -> h
md (T t) = t
md Z = ""

instance Xml Md where
  dcl e = e
  element = \case
    "p" -> BE $ P ""
    "h1" -> BE $ H1 ""
    "h2" -> BE $ H2 ""
    "pre" -> BE $ Pre ""
    "code" -> IE $ Code ""
    "em" -> IE $ Em ""
    "a" -> IE $ Link "" ""
    _ -> Z
  attribute = \case
    "href" -> T "href"
    _ -> Z
  value = T
  string = T
  empty = Z

  BE a .# BE b = T $ md (BE a) <> "\n" <> md (BE b)
  BE a .# IE b = BE a .# T (md (IE b))
  IE a .# BE b = T $ md (IE a) <> "\n" <> md (BE b)
  IE a .# IE b = T $ md (IE a) <> md (IE b)
  BE a .# T b = T $ md (BE a) <> "\n" <> b
  IE a .# T b = T $ md (IE a) <> b
  T a .# BE b = T $ a <> "\n" <> md (BE b)
  T a .# IE b = T $ a <> md (IE b)
  T a .# T b = T $ a <> b
  Z .# b = b
  a .# Z = a

  BE a .> b = case a of
    P t -> BE $ P (t <> md b)
    H1 t -> BE $ H1 (t <> md b)
    H2 t -> BE $ H2 (t <> md b)
    Pre t -> BE $ Pre (t <> md b)
  IE a .> b = case a of
    Code t -> IE $ Code (t <> md b)
    Em t -> IE $ Em (t <> md b)
    Link h _ -> IE $ Link h (md b)
    Href h -> IE $ Href h
  T a .> b = T a .# b
  Z .> b = b

  a .@ IE (Href b) = case a of
    IE (Link _ _) -> IE $ Link b (md a)
    _ -> a
  a .@ _ = a

  IE (Href h) .+ _ = IE (Href h)
  _ .+ IE (Href h) = IE (Href h)
  _ .+ _ = Z

  T "href" .= b = IE (Href (md b))
  _ .= _ = Z
