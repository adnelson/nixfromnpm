{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module NixFromNpm.Conversion.ToNix where

import qualified Prelude as P
import qualified Data.HashMap.Strict as H
import Data.Text (Text, replace)

import Data.SemVer

import NixFromNpm.Common hiding (replace)
import Nix.Types hiding (mkPath)
import Nix.Pretty (prettyNix)
import qualified Nix.Types as Nix
import Nix.Parser

import NixFromNpm.Npm.Types
import NixFromNpm.Npm.PackageMap

-- | This contains the same information as the .nix file that corresponds
-- to the package. More or less it tells us everything that we need to build
-- the package.
data ResolvedPkg = ResolvedPkg {
  rpName :: PackageName,
  rpVersion :: SemVer,
  rpDistInfo :: Maybe DistInfo,
  rpToken :: Maybe AuthToken,
  -- ^ If a token was necessary to fetch the package, include it here.
  rpMeta :: PackageMeta,
  rpDependencies :: PRecord ResolvedDependency,
  rpPeerDependencies :: PRecord ResolvedDependency,
  rpOptionalDependencies :: PRecord ResolvedDependency,
  rpDevDependencies :: Maybe (PRecord ResolvedDependency)
  } deriving (Show, Eq)

callPackage :: NExpr -> NExpr
callPackage = callPackageWith []

callPackageWith :: [Binding NExpr] -> NExpr -> NExpr
callPackageWith args e = mkApp (mkApp (mkSym "callPackage") e)
                               (mkNonRecSet args)

-- | Turns a string into one that can be used as an identifier.
-- NPM package names can contain dots, so we translate these into dashes.
fixName :: Name -> Name
fixName = replace "." "-"

-- | Turn a PackageName into a valid nix identifier.
fixPackageName :: PackageName -> Name
fixPackageName (PackageName name Nothing) = fixName name
fixPackageName (PackageName name (Just namespace)) =
  concat ["__priv-", namespace, "-", fixName name]

-- | Converts a package name and semver into an identifier.
-- Example: "foo" and 1.2.3 turns into "foo_1-2-3".
-- Example: "foo.bar" and 1.2.3-baz turns into "foo-bar_1-2-3-baz"
toDepName :: PackageName -> SemVer -> Name
toDepName name (a, b, c, tags) = do
  let suffix = pack $ intercalate "-" $ (map show [a, b, c]) <> map unpack tags
  fixPackageName name <> "_" <> suffix

-- | Converts a ResolvedDependency to a nix expression.
toNixExpr :: PackageName -> ResolvedDependency -> NExpr
toNixExpr name (Resolved semver) = mkSym $ toDepName name semver
toNixExpr name (Broken reason) = mkApp (mkSym "brokenPackage") $ mkNonRecSet
  [ "name" `bindTo` str (pshow name), "reason" `bindTo` str (pshow reason)]

-- | Write a nix expression pretty-printed to a file.
writeNix :: MonadIO io => FilePath -> NExpr -> io ()
writeNix path = writeFile path . show . prettyNix

-- | Gets the .nix filename of a semver. E.g. (0, 1, 2) -> 0.1.2.nix
toDotNix :: SemVer -> FilePath
toDotNix v = fromText $ renderSV v <> ".nix"

-- | Creates a doublequoted string from some text.
str :: Text -> NExpr
str = mkStr DoubleQuoted

-- | Converts distinfo into a nix fetchurl call.
distInfoToNix :: Maybe AuthToken -> Maybe DistInfo -> NExpr
distInfoToNix _ Nothing = Nix.mkPath False "./."
distInfoToNix maybToken (Just DistInfo{..}) = do
  let Success fetchurl = case maybToken of
        Nothing -> parseNixString "pkgs.fetchurl"
        Just _ -> parseNixString "pkgs.fetchurlWithHeaders"
      (algo, hash) = case diShasum of
        SHA1 hash' -> ("sha1", hash')
        SHA256 hash' -> ("sha256", hash')
      authBinding = case maybToken of
        Nothing -> []
        Just auth -> do
          let -- Equivalent to "headers.Authentication ="
            binder = NamedVar [StaticKey "headers",
                               StaticKey "Authentication"]
            header = "Bearer " <> decodeUtf8 auth
            -- Equiv. to 'headers.Authentication = "Bearer <token>"'
            binding = binder $ str header
          [binding]
      bindings =
        ["url" `bindTo` str diUrl, algo `bindTo` str hash]
        <> authBinding
  fetchurl `mkApp` mkNonRecSet bindings

-- | Converts package meta to a nix expression, if it exists.
metaToNix :: PackageMeta -> Maybe NExpr
metaToNix PackageMeta{..} = do
  let
    grab name = maybe [] (\s -> [name `bindTo` str s])
    homepage = grab "homepage" (map uriToText pmHomepage)
    description = grab "description" pmDescription
    author = grab "author" pmAuthor
    keywords = case pmKeywords of
      ks | null ks -> []
         | otherwise -> ["keywords" `bindTo` mkList (toList (map str ks))]
  case homepage <> description <> keywords <> author of
    [] -> Nothing
    bindings -> Just $ mkNonRecSet bindings

hasBroken :: ResolvedPkg -> Bool
hasBroken ResolvedPkg{..} = case rpDevDependencies of
  Nothing -> any isBroken rpDependencies
  Just devDeps -> any isBroken rpDependencies || any isBroken devDeps
  where isBroken = \case {Broken _ -> True; _ -> False}

-- | Converts a resolved package object into a nix expression. The expresion
-- will be a function where the arguments are its dependencies, and its result
-- is a call to `buildNodePackage`.
resolvedPkgToNix :: ResolvedPkg -> NExpr
resolvedPkgToNix rPkg@ResolvedPkg{..} = do
  let -- Get a string representation of each dependency in name-version format.
      deps = map (uncurry toNixExpr) $ H.toList rpDependencies
      peerDeps = map (uncurry toNixExpr) $ H.toList rpPeerDependencies
      optDeps = map (uncurry toNixExpr) $ H.toList rpOptionalDependencies
      -- Same for dev dependencies.
      devDeps = map (uncurry toNixExpr) . H.toList <$> rpDevDependencies
      -- | List of arguments that these functions will take.
      funcParams' = catMaybes [
        Just "pkgs",
        Just "buildNodePackage",
        Just "nodePackages",
        maybeIf (hasBroken rPkg) "brokenPackage"
        ]
      -- None of these have defaults, so put them into pairs with Nothing.
      funcParams = mkFormalSet $ map (\x -> (x, Nothing)) funcParams'
      -- Wrap an list expression in a `with nodePackages;` syntax if non-empty.
      withNodePackages noneIfEmpty list = case list of
        [] -> if noneIfEmpty then Nothing else Just $ mkList []
        _ -> Just $ mkWith (mkSym "nodePackages") $ mkList list
  let devDepBinding = case devDeps of
        Nothing -> Nothing
        Just ddeps -> bindTo "devDependencies" <$> withNodePackages False ddeps
      PackageName name namespace = rpName
  let args = mkNonRecSet $ catMaybes [
        Just $ "name" `bindTo` str name,
        Just $ "version" `bindTo` (str $ renderSV rpVersion),
        Just $ "src" `bindTo` distInfoToNix rpToken rpDistInfo,
        bindTo "namespace" <$> map str namespace,
        bindTo "deps" <$> withNodePackages False deps,
        bindTo "peerDependencies" <$> withNodePackages True peerDeps,
        bindTo "optionalDependencies" <$> withNodePackages True optDeps,
        devDepBinding,
        bindTo "meta" <$> metaToNix rpMeta
        ]
  mkFunction funcParams $ mkSym "buildNodePackage" `mkApp` args


-- | Convenience function to generate an `import /path {args}` expression.
importWith :: Bool -- ^ True if the path is from the env, e.g. <nixpkgs>
           -> FilePath -- ^ Path to import
           -> [Binding NExpr] -- ^ Arguments to pass
           -> NExpr -- ^ The resulting nix expression
importWith isEnv path args = do
  mkSym "import" `mkApp` Nix.mkPath isEnv (pathToString path)
                 `mkApp` mkNonRecSet args

-- | We use this a few times: `import <nixpkgs> {}`
importNixpkgs :: NExpr
importNixpkgs = importWith True "nixpkgs" []

-- | Also used a few times, these are the top-level params to the generated
-- default.nix files.
defaultParams :: Formals NExpr
defaultParams = mkFormalSet [("pkgs", Just importNixpkgs),
                             ("nodejsVersion", Just $ str "4.1")]

-- | When passing through arguments, we inherit these two things.
defaultInherits :: [Binding NExpr]
defaultInherits = [Inherit Nothing $ map mkSelector ["pkgs", "nodejsVersion"]]

-- | The name of the subfolder within the output directory that
-- contains node packages.
nodePackagesDir :: FilePath
nodePackagesDir = "nodePackages"

bindRootPath :: Binding NExpr
bindRootPath = "rootPath" `bindTo` mkPath ("./" </> nodePackagesDir)

-- | The root-level default.nix file, which does not have any extensions.
rootDefaultNix :: NExpr
rootDefaultNix = mkFunction defaultParams body where
  nodeLibArgs = defaultInherits <> ["self" `bindTo` mkSym "nodeLib"]
  lets = ["nodeLib" `bindTo` importWith False "./nodeLib" nodeLibArgs]
  Success genPackages = parseNixString "nodeLib.generatePackages"
  body = mkLet lets $ mkApp genPackages (mkNonRecSet [bindRootPath])

-- | Creates the `default.nix` file that is the top-level expression we are
-- generating.
defaultNixExtending :: Name -- ^ Name of first extension.
                    -> Record FilePath -- ^ Extensions being included.
                    -> NExpr -- ^ A generated nix expression.
defaultNixExtending extName extensions = do
  mkFunction defaultParams body where
    -- Map over the expression map, creating a binding for each pair.
    lets = flip map (H.toList extensions) $ \(name, path) -> do
      -- Equiv. to `name = import /path {inherit pkgs nodejsVersion;}`
      name `bindTo` importWith False path defaultInherits
    args = [bindRootPath, "extensions" `bindTo`
              mkList (map mkSym (H.keys extensions))]
    genPkgs = extName <> ".nodeLib.generatePackages"
    Success generatePackages = parseNixText genPkgs
    body = mkLet lets $ mkApp generatePackages (mkNonRecSet args)

-- | Create a `default.nix` file for a particular package.json; this simply
-- imports the package as defined in the given path, and calls into it.
packageJsonDefaultNix :: FilePath -- ^ Path to the output directory.
                      -> NExpr
packageJsonDefaultNix outputPath = do
  let
    libBind = "lib" `bindTo` importWith False outputPath defaultInherits
    Success callPkg = parseNixString "lib.callPackage"
    call = callPkg `mkApp` mkPath "project.nix" `mkApp` mkNonRecSet []
  mkFunction defaultParams $ mkLet [libBind] call

bindingsToMap :: [Binding t] -> Record t
bindingsToMap = foldl' step mempty where
  step record binding = case binding of
    NamedVar [StaticKey key] obj -> H.insert key obj record
    _ -> record
