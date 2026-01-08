{-# LANGUAGE OverloadedStrings #-}
module Run where
import DryRun (DryRun (..), dryRun)
import System.FilePath (takeDirectory)
import System.Directory (createDirectoryIfMissing)
import Codec.Archive.Zip (createArchive, packDirRecur, addEntry, mkEntrySelector, CompressionMethod (..), withArchive)

run :: DryRun -> IO ()
run dr = do
  mapM_ g fs
  m <- mkEntrySelector "mimetype"
  createArchive fn (addEntry Store "application/epub+zip" m)
  withArchive fn (packDirRecur Deflate mkEntrySelector dir)
  where
    (fn, dir) = epubF dr
    fs = map (\(n, c) -> (dir ++ "/" ++ n, c)) . files $ dr
    g (n, c) = do
      let d = takeDirectory n
      createDirectoryIfMissing True d
      writeFile n c

createEpub :: DryRun -> IO ()
createEpub = run . dryRun
