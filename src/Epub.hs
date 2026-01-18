{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Epub where
import System.Directory (createDirectoryIfMissing, copyFile, listDirectory, doesDirectoryExist, removeDirectoryRecursive, removeFile)
import Codec.Archive.Zip (createArchive, addEntry, CompressionMethod (..), mkEntrySelector, packDirRecur, withArchive)
import System.FilePath (takeDirectory, (</>))
import Control.Monad (forM_)
import Xml (view, Xml (..), Element, attrs, (#))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO as T ( writeFile )

data Output = Output { odir :: String, oname :: String, oassets :: String }

genEpub :: Output -> [Metadata] -> [Asset] -> [XhtmlFile] -> [SpineItem] -> IO ()
genEpub o md as xs ys = do
  genEpub' o md as xs ys
  zipF o

genEpub' :: Output -> [Metadata] -> [Asset] -> [XhtmlFile] -> [SpineItem] -> IO ()
genEpub' o md as xs ys = do
  -- Create output directory
  createDirectoryIfMissing True (odir o)
  removeAllFilesUnderDir (odir o)

  -- Copy asset files
  mapM_ (copy (oassets o) (odir o </> "OEBPS") . T.unpack . ahref) as
  -- Generate container.xml
  genContainerXml
  -- Generate content.opf
  let md' = metadataXml md
      ms = manifest as xs
      sp = spine ys
  genF "content.opf" (view (opf md' ms sp))
  -- Generate xhtml files
  mapM_ (\x -> genF (xhref x) (xtext x)) xs

zipF :: Output -> IO ()
zipF o = do
  -- mimetype has to be the first entry of the zip file.
  s <- mkEntrySelector "mimetype"
  let
    p :: String
    p = oname o <> ".epub"
  createArchive p (addEntry Store "application/epub+zip" s)
  withArchive p (packDirRecur Deflate mkEntrySelector ".output-epub")

data Metadata = Title Text | Identifier Text | Lang Text | Modified Text | Rights Text | Creator Text | Version Text | SpecifiedFonts Bool | CoverImage Text

data Asset = Asset { aid :: Text, ahref :: Text, amt :: Text, ap :: Maybe Text }

data XhtmlFile = XhtmlFile { xid :: Text, xhref :: Text, xp :: Maybe Text, xtext :: Text }

data SpineItem = SpineItem { sid :: Text, linear :: Bool }

cssA :: Text -> Text -> Asset
cssA aid ahref = Asset { aid, ahref, amt = "text/css", ap = Nothing } 

fontA :: Text -> Text -> Asset
fontA aid ahref = Asset { aid, ahref, amt = "application/x-font-ttf", ap = Nothing } 

audioA :: Text -> Text -> Asset
audioA aid ahref = Asset { aid, ahref, amt = "audio/mp4", ap = Nothing } 

videoA :: Text -> Text -> Asset
videoA aid ahref = Asset { aid, ahref, amt = "video/mp4", ap = Nothing } 

pdfA :: Text -> Text -> Asset
pdfA aid ahref = Asset { aid, ahref, amt = "application/pdf", ap = Nothing } 

jpgA :: Text -> Text -> Asset
jpgA aid ahref = Asset { aid, ahref, amt = "image/jpeg", ap = Nothing } 

pngA :: Text -> Text -> Asset
pngA aid ahref = Asset { aid, ahref, amt = "image/png", ap = Nothing } 

assetXml :: Xml repr => Asset -> repr Element
assetXml x = element "item" .@ (attrs [("id", aid x), ("href", ahref x), ("media-type", amt x)] .+
    case ap x of
        Nothing -> Xml.empty
        Just p -> attribute "properties" .= value p)

xhtmlA :: Text -> Text -> Text -> XhtmlFile
xhtmlA xid xhref xtext = XhtmlFile { xid, xhref, xtext, xp = Nothing }

xhtmlA' :: Text -> Text -> Text -> Text -> XhtmlFile
xhtmlA' xid xhref xp xtext = XhtmlFile { xid, xhref, xtext, xp = Just xp }

xhtmlXml :: Xml repr => XhtmlFile -> repr Element
xhtmlXml x = let
    p = case xp x of
        Nothing -> []
        Just v -> [("properties", v)]
    in element "item" .@ attrs ([("id", xid x), ("href", xhref x), ("media-type", "application/xhtml+xml")] ++ p)

mitem :: Text -> Text -> Text -> Text -> Asset
mitem aid ahref amt ap = Asset { aid, ahref, amt, ap = Just ap }

manifest :: Xml repr => [Asset] -> [XhtmlFile] -> repr Element
manifest xs ys = element "manifest" .> foldr (#) Xml.empty (map assetXml xs <> map xhtmlXml ys)

genF :: Text -> Text -> IO ()
genF h c = createFileWithDirs (".output-epub/OEBPS" </> T.unpack h) c

copy :: String {- source dir -} -> String {- destination dir -} -> String {- source path -} -> IO ()
copy sd dd s = copyFileCreateDir (sd ++ "/" ++ s) (dd ++ "/" ++ s)

-- | Copies a file to a destination, creating the destination directory
-- and any missing parent directories along the way.
copyFileCreateDir :: FilePath -> FilePath -> IO ()
copyFileCreateDir sourcePath destinationPath = do
    let destDir = takeDirectory destinationPath
    -- Create the directory (and parents) if they don't exist
    createDirectoryIfMissing True destDir
    -- Copy the file
    copyFile sourcePath destinationPath

genContainerXml :: IO ()
genContainerXml = createFileWithDirs ".output-epub/META-INF/container.xml" (view containerXml)

-- | Removes all files and subdirectories within a given directory 'dir', 
--   but keeps 'dir' itself.
removeAllFilesUnderDir :: FilePath -> IO ()
removeAllFilesUnderDir dir = do
    contents <- listDirectory dir
    forM_ contents $ \item -> do
        let fullPath = dir </> item
        isDir <- doesDirectoryExist fullPath
        if isDir
            -- Recursively remove directory contents. Symlinks within are removed without affecting targets.
            then removeDirectoryRecursive fullPath
            -- Remove file
            else removeFile fullPath
    -- Catches potential errors if something disappears during the process (e.g., race condition)
    -- `catch` (\e -> putStrLn $ "Error during removal: " ++ show (e :))

createFileWithDirs :: FilePath -> Text -> IO ()
createFileWithDirs filePath content = do
    let dir = takeDirectory filePath
    -- Create parent directories recursively (first argument True)
    createDirectoryIfMissing True dir
    -- Write the file (will create the file if it doesn't exist)
    T.writeFile filePath content


containerXml :: Xml repr => repr Element
containerXml = dcl $ element "container" .@ (attribute "version" .= value "1.0" .+ attribute "xmlns" .= value "urn:oasis:names:tc:opendocument:xmlns:container") .> element "rootfiles" .> element "rootfile" .@ (attribute "full-path" .= value "OEBPS/content.opf" .+ attribute "media-type" .= value "application/oebps-package+xml")

spi :: Text -> SpineItem
spi sid = SpineItem { sid, linear = True }

spi' :: Text -> SpineItem
spi' sid = SpineItem { sid, linear = False }

spiXml :: Xml repr => SpineItem -> repr Element
spiXml x
    | linear x = element "itemref" .@ (attribute "idref" .= value (sid x))
    | otherwise = element "itemref" .@ (attribute "idref" .= value (sid x) .+ attribute "linear" .= value "no")

spine :: Xml repr => [SpineItem] -> repr Element
spine = (element "spine" .>) . Prelude.foldr (#) Xml.empty . map spiXml

opf :: Xml repr => repr Element -> repr Element -> repr Element -> repr Element
opf md mf sp = dcl $ element "package" .@ attrs [("xmlns","http://www.idpf.org/2007/opf"),("unique-identifier", "bookid-3.3.0") ,("version", "3.0") , ("prefix", "rendition: http://www.idpf.org/vocab/rendition/# ibooks: http://vocabulary.itunes.apple.com/rdf/ibooks/vocabulary-extensions-1.0/") , ("xmlns:epub", "http://www.idpf.org/2007/ops") , ("xmlns:ibooks", "http://vocabulary.itunes.apple.com/rdf/ibooks/vocabulary-extensions-1.0/")]
    .> (md # mf # sp)

metadataXml :: Xml repr => [Metadata] -> repr Element
metadataXml xs = element "metadata" .@ attrs [("xmlns:opf", "http://www.idpf.org/2007/opf"), ("xmlns:dc", "http://purl.org/dc/elements/1.1/")]
        .> (foldr (#) (Xml.empty) $ map meta xs)
    where
        meta = \case
            Title s -> element "dc:title" .> string s
            Identifier s -> element "dc:identifier" .@ (attribute "id" .= value "bookid-3.3.0") .> string s
            Lang s -> element "dc:language" .> string s
            Modified s -> element "meta" .@ (attribute "property" .= value "dcterms:modified") .> string s
            Rights s -> element "dc:rights" .> string s
            Creator s -> element "dc:creator" .> string s
            Version s -> element "meta" .@ (attribute "property" .= value "ibooks:version") .> string s
            SpecifiedFonts b -> element "meta" .@ (attribute "property" .= value "ibooks:specified-fonts") .> string (if b then "true" else "false")
            CoverImage aid -> element "meta" .@ (attribute "cover" .= value aid)
