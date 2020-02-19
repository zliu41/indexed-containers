#!/usr/bin/env stack
-- stack --resolver lts-15.0 script --package filepath --package directory --package extra
-- To run: ./TestGen.hs

module TestGen (main) where

import Data.List.Extra (replace, stripPrefix, trim)
import Data.Maybe (mapMaybe)
import System.Directory
import System.FilePath

import Prelude hiding (mod)

main :: IO ()
main = genTestsFor "Data.NList"

genTestsFor :: String -> IO ()
genTestsFor mod = do
  let inputFile = "src" </> replace "." [pathSeparator] mod <.> "hs"
      outputFile = "test/hspec" </> (replace "." [pathSeparator] mod ++ "Spec.hs")
  src <- readFile inputFile
  createDirectoryIfMissing True (takeDirectory outputFile)
  let lns = fmap trim (lines src)
      tests = mapMaybe (stripPrefix "-- > ") lns
  writeFile outputFile . unlines $ header mod ++ fmap (indent 6) tests

header :: String -> [String]
header mod =
  [ "-- Generated code, do not modify by hand. Generate by running TestGen.hs."
  , ""
  , "{-# LANGUAGE DataKinds #-}"
  , "{-# LANGUAGE TypeApplications #-}"
  , "{-# LANGUAGE TypeFamilies #-}"
  , "{-# OPTIONS_GHC -w #-}"
  , "module " ++ mod ++ "Spec where"
  , ""
  , "import Test.Hspec"
  , "import Prelude hiding (concat, drop, head, init, last, length, replicate, reverse, splitAt, tail, take, unzip, zip, zipWith)"
  ] ++ ["import " ++ mod] ++
  [ ""
  , "infix 4 ==="
  , "(===) :: (HasCallStack, Show a, Eq a) => a -> a -> Expectation"
  , "(===) = shouldBe"
  , ""
  , "spec :: Spec"
  , "spec = do"
  , "  describe \"Testing " ++ mod ++ "\" $ do"
  , "    it \"\" $ do"
  ]

indent :: Int -> String -> String
indent n = (replicate n ' ' ++)
