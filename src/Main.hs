{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
import Control.Applicative
import Data.Text         (Text, unpack, pack)
import Data.Text.IO as T (putStrLn)
import Data.String
import Data.Monoid       ((<>))
import Data.Char

import qualified GitHub.Endpoints.Issues as GHI
import qualified GitHub.Data.Id as GHD

import Options.Generic

data CommandLine w
  = Title
    { id :: w ::: Int <?> "The issue number to request"
    , underscores :: w ::: Bool <?> "Convert name to underscores?"
    , repo :: w ::: String <?> "For example: rust-lang/rust"
    }
  | Code
    { id :: w ::: Int <?> "The issue number to request"
    , repo :: w ::: String <?> "For example: rust-lang/rust"
    }
  deriving (Generic)

instance ParseRecord (CommandLine Wrapped)
deriving instance Show (CommandLine Unwrapped)


main :: IO ()
main = do
  cmdline <- unwrapRecord "Buhgit - command-line GitHub issue manager"
  dispatchWork cmdline

parseRepo :: String -> (String, String)
parseRepo str
  = (a, filter f b)
  where
    (a, b) = span f str
    f x = x /= '/'

dispatchWork :: CommandLine Unwrapped -> IO ()
dispatchWork (Title id underscores _repo) = do
  let (user, repo) = parseRepo _repo
  Right theIssue <- GHI.issue (fromString user) (fromString repo) (GHD.Id id)
  let str = unpack $ GHI.issueTitle theIssue
  if underscores then
    Prelude.putStrLn $
      map (toLower . repl_us)
        (filter (liftA2 (||) isAlphaNum isSpace) str)
  else
    Prelude.putStrLn str
  where
    repl_us ' ' = '_'
    repl_us a   = a
dispatchWork (Code id r) = do
  let (user, repo) = parseRepo r
  Right theIssue <- GHI.issue (fromString user) (fromString repo) (GHD.Id id)
  case GHI.issueBody theIssue of
    Just text ->
      Prelude.putStrLn (unpack text)
    Nothing ->
      Prelude.putStrLn "Error: issue has no body"
