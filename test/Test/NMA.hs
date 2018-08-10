module Test.NMA where

import           Data.Maybe

import           Data.Aeson
import qualified Data.ByteString.Lazy             as B
import qualified Data.Map.Strict                  as Map

import           TestHS

import           Data.NMA

{-fastTests :: [Test]-}
{-fastTests = [ -}
              {-test1-}
            {-]-}

ioTests :: [IO Test]
ioTests = [ test1
          ]

test1 :: IO Test
test1 = do
  let name = "read hatmatrix json"
  let hmat = "test/big_widehat.json"
  let getJSON = B.readFile hmat
  ehmrow <- (eitherDecode <$> getJSON) :: IO (Either String HatMatrixRaw)
  case ehmrow of
    Left err -> return $ testFailed name $ ("error json parse",err)
    Right hmrow -> do
      let hatmatrix = hatMatrixFromList hmrow
      putStrLn "row names"
      return $ testPassed name $ "passed!"
