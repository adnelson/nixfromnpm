{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module NixFromNpm.NpmVersion where

import NixFromNpm.Common
import NixFromNpm.SemVer

data GitSource = Github | Bitbucket | Gist | GitLab deriving (Show, Eq)

data NpmVersionRange
  = SemVerRange SemVerRange
  | Tag Name
  | NpmUri URI
  | GitId GitSource Name Name (Maybe Name)
  | LocalPath FilePath
  deriving (Show, Eq)
