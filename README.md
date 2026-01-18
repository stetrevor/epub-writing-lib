# How to Use

This is what a complete XHTML file looks like:

```
chapter1 :: Xml repr => repr Element
chapter1 = dcl $ element "html" .@
    attribute "xmlns" .= value "http://www.w3.org/1999/xhtml"
  .> (
      element "head" .> element "title" .> string "Chapter 1"
      # element "body" .> element "h1" .> string "Chapter 1 Title"
      # element "p" .> string "This is the first chapter."
      )

```

You can define your own XHTML files using the same combinators.

This is everything you need to create an EPUB:

```
output :: Output
output = Output { odir = ".output-epub", oname = "demo", oassets = "./app/assets" }

metadata :: [Metadata]
metadata = [
      Title "Apple Books EPUB Example v3.3"
    , Identifier "0123456789"
    , Lang "en"
    , Modified "2024-05-01T01:00:00Z"
    , Rights "This book is copyrighted by Apple Inc and must not be shared."
    , Creator "Apple Inc."
    , Version "3.3"
    , SpecifiedFonts True
    ]

assets :: [Asset]
assets = [
      audioA "aud1" "audio/Hot_Air_Audio.m4a"
    , fontA "font-DrukWide" "fonts/DrukWide-Medium.ttf"
    , pngA "img3" "images/balloon_data.png"
    , jpgA "frequency" "images/frequency.jpg"
    , mitem "cover-image" "images/cover-image.png" "image/png" "cover-image"
    , pdfA "pdf" "pdf/luftschiffahrt.pdf"
    , videoA "vid1" "video/inflating-balloon.mp4"
    , cssA "css-cht" "css/balloon_data.css"
    ]

xhtmls :: [XhtmlFile]
xhtmls = [
      xhtmlA "chart" "balloon_data.xhtml" $ view balloon_dataXhtml
    , xhtmlA "bib" "bibliography.xhtml" $ view bibliographyXhtml
    , xhtmlA "j2026_01_07" "2026_01_07.xhtml" $ view journal_2026_01_07
    , xhtmlA' "chapter1" "chapter1.xhtml" "svg" $ view chapter1Xhtml
    , xhtmlA "chapter2" "chapter2.xhtml" $ view chapter2Xhtml
    , xhtmlA "cover" "cover.xhtml" $ view coverXhtml
    , xhtmlA "end" "endnotes.xhtml" $ view endnotesXhtml
    , xhtmlA' "toc" "toc.xhtml" "nav" $ view tocXhtml
    ]

sps :: [SpineItem]
sps = [spi "cover" 
    , spi "chapter1"
    , spi "chapter2"
    , spi "j2026_01_07"
    , spi' "end"
    , spi "bib"
    , spi' "chart"
    ]

book :: IO ()
book = genEpub output metadata assets xhtmls sps

```

First, you specify the output parameters, then you create the metadata list, then the static assets, and all the xhtml files, and the spine section of the package document. Finally you can call `genEpub` to generate the EPUB file.

# What is This Library and Why

This is a library to aid in EPUB book writing. This is ideal if your epub is flowing layout or simple pages of pictures. It provides an embedded DSL(Domain Specific Language) for authoring XHTML file contents. It also provides functions to generate the package document and container file required for EPUB files.

Writing XHTML can get tedious very quickly. There are many markup languages to simplify the process. But as I used them before, I often found them falling short of what I wanted to express.

## Markdown vs This Library 

Languages like markdown has limited set of features. Trying to add classes and id support are not easy.

This library provides a set of combinators to express XHTML structure. You can create elements, attributes, and values. You can nest elements and add attributes to them. This gives you the full power of XHTML without the verbosity of raw XML. Writing XML or XHTML is writing Haskell functions. 

## DSL for XML

```
class Xml repr where
  dcl :: repr Element -> repr Element
  (#) :: repr Element -> repr Element -> repr Element
  (.>) :: repr Element -> repr Element -> repr Element
  (.@) :: repr Element -> repr Attribution -> repr Element
  (.=) :: repr Attribute -> repr Value -> repr Attribution
  element :: Text {- name -} -> repr Element
  attribute :: Text {- name -} -> repr Attribute
  value :: Text {- value -} -> repr Value
  string :: Text -> repr Element
  empty :: repr a

```

The DSL uses final encoding. It’s inspired by *[type-of-html](https://hackage.haskell.org/package/type-of-html)*. This DSL defines the syntax in this way: 

`.>` is a combinator from a parent element to a child element. 

`.@` connects an element with its attributions. 

`.=` assigns a value to an attribute. 

`dcl` is the the XML declaration line. Although its type allows it to be used anywhere in a document, its correct use is at the outmost of everything else. 

`element` represents a tag. 

`attribute` and `value` are self explanatory. 

`empty` provides an escape hatch for when nothing needs to be represented but an Element, Attribution, Attribute or Value is still needed. 

## Initial Encoding vs Final Encoding

In *type-of-html*, every tag name, attribute name or attribute value is defined and exactly typed. This has two issues when it comes to hand writing html.

First is that the final type is very long and unwieldy. Although with IDE, getting the correct type is easier, but the type alone can still span multiple lines for a typical HTML document. And every time you make a change in anything, the type needs to be updated to reflect that change.

Second is there is no way to use any element or attribution from other namespaces other than HTML. This makes it impossible to author XHTML and XML at the same time. 

Our final encoding here has no such issues. The type of the document is `xml repr => repr Element` always. 

And you are free to use any elements and attributions from any namespaces. 

## The Real Power of the DSL

Apart from the features mentioned above. Those are things you can do with the DSL:

DNRIY: if you use any thing often, write a function for it, this is handy when you have predefined styles for things like titles and pictures. 

A template for a document is just another function. You can write it yourself. 

Another things you can do is element generation with state. You can generate sequential ids for anchors in a document: 

Another things you can do is element generation with state. You can generate sequential ids for anchors in a document: 

```
newtype R repr a = R { unR :: State Int (repr a) }

instance Xml repr => Xml (R repr) where
  dcl e = R $ unR e >>= return . dcl
  a # b = R $ liftA2 (#) (unR a) (unR b)
  a .> b = R $ liftA2 (.>) (unR a) (unR b)
  a .@ b = R $ liftA2 (.@) (unR a) (unR b)
  a .= b = R $ liftA2 (.=) (unR a) (unR b)
  element = R . return . element
  attribute = R . return . attribute
  value s
    | s == idmark = R $ do
      i <- get
      put (i + 1)
      return (value (anchor i))
    | otherwise = R $ return (value s)
  string = R . return . string
  empty = R (return Xml.empty)

addAnchors :: R repr a -> repr a
addAnchors e = evalState (unR e) 1

idmark :: Text
idmark = "__GENERATE_ID__"

anchor :: Int -> Text
anchor i = "anchor-" <> T.pack (show i)

```

## EPUB Output as a Bonus

An EPUB is a collection of xml files, XHTML files and static assets like pdf, images, audios or videos. Th DSL can represent everything that&rsquo;s XML. The EPUB generation is just a thin wrapper to collect output parameters. It needs to know the metadata, the static assets it contains, the XHTML files it contains to generate the package document and put everything in correct folders. Then just zip everything according to EPUB specifications. 

## Where is the mimetype File

Although this is an implementation detail, the *mimetype* only exists directly in the archived epub file. It&rsquo;s not generated in the output folder. Simply zipping the output folder, even with the correct *mimetype* file present, will not produce a correct epub file. 

## EPUB Checker

To verify everything is in order, the [epub checker](https://www.w3.org/publishing/epubcheck/) from w3c.org is recommended. 

## Generated XHTML is Unreadable

The generated XHTML files contains no indentation or new lines unnecessary. This makes no difference in the epub output. To read the generated files, a formatter from your editor is recommended. And using the `zipF` function provided, the any changes you make in the output folder will make it to the generated epub. 

Why not formatting the files during generation? XHTML is after all whitespace sensitive. While it&rsquo;s possible to make an educated guess at what tags are not affected by whitespaces, and indent them accordingly, CSS still can alter the display of an element. It is beyond the scope of this project to produce readable XHTML files. Our goal is to produce readable EPUB, with the least amount of pain possible. 
