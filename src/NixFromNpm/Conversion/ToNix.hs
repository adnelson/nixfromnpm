{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
module NixFromNpm.Conversion.ToNix where

import qualified Prelude as P
import Data.Fix (Fix(..))
import qualified Data.HashMap.Strict as H
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Char8 as C8
import Data.Char (isDigit)
import Data.Text (Text, replace)
import qualified Data.Text as T

import Data.SemVer

import NixFromNpm.Common hiding (replace)
import Nix.Expr hiding (mkPath)
import Nix.Pretty (prettyNix)
import qualified Nix.Expr as Nix
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
  -- ^ If a token was necessary to fetch the package, include it here.
  rpMeta :: PackageMeta,
  rpDependencies :: PRecord ResolvedDependency,
  rpOptionalDependencies :: PRecord ResolvedDependency,
  rpDevDependencies :: Maybe (PRecord ResolvedDependency)
  } deriving (Show, Eq)

-- | True if any of the package's dependencies have namespaces.
hasNamespacedDependency :: ResolvedPkg -> Bool
hasNamespacedDependency rPkg = any hasNs (allDeps rPkg) where
  -- Get all of the dependency sets of the package.
  allDeps ResolvedPkg{..} = [rpDependencies,
                             rpOptionalDependencies,
                             maybe mempty id rpDevDependencies]
  -- Look at all of the package names (keys) to see if any are namespaced.
  hasNs = any isNamespaced . H.keys

-- | Turns a string into one that can be used as an identifier.
-- NPM package names can contain dots, so we translate these into dashes.
-- Names can also start with a number; in this case prefix with an underscore.
fixName :: Name -> Name
fixName name = do
  -- Replace dots with dashes
  let name' = replace "." "-" name
  case T.findIndex isDigit name' of
    -- First character is a digit; prefix with underscore
    Just 0 -> "_" <> name'
    _ -> name'

-- | Converts a package name and semver into an Nix expression.
-- Example: "foo" and 1.2.3 turns into "foo_1-2-3".
-- Example: "foo.bar" and 1.2.3-baz turns into "foo-bar_1-2-3-baz"
-- Example: "@foo/bar" and 1.2.3 turns into "namespaces.foo.bar_1-2-3"
toDepExpr :: PackageName -> SemVer -> NExpr
toDepExpr (PackageName name mNamespace) (SemVer a b c (PrereleaseTags tags) _) = do
  let suffix = pack $ intercalate "-" $ (map show [a, b, c]) <> map show tags
      ident = fixName name <> "_" <> suffix
  case mNamespace of
    Nothing -> mkSym ident
    -- If there's a namespace, call "namespaces.namespace.pkgname"
    Just namespace -> mkDots "namespaces" [namespace, ident]

-- | Converts a package name and semver into an Nix selector, which can
-- be used in a binding. This is very similar to @toDepExpr@, but it returns
-- something to be used in a binding rather than an expression.
toSelector :: PackageName -> SemVer -> NAttrPath NExpr
toSelector (PackageName name mNamespace) (SemVer a b c (PrereleaseTags tags) _) = do
  let suffix = pack $ intercalate "-" $ (map show [a, b, c]) <> map show tags
      ident = fixName name <> "_" <> suffix
  StaticKey <$> case mNamespace of
    Nothing -> [ident]
    Just namespace -> ["namespaces", namespace, ident]

-- | Same as toSelector, but doesn't append a version.
toSelectorNoVersion :: PackageName -> NAttrPath NExpr
toSelectorNoVersion (PackageName name mNamespace) = do
  StaticKey <$> case mNamespace of
    Nothing -> [fixName name]
    Just namespace -> ["namespaces", namespace, fixName name]

-- | Converts a ResolvedDependency to a nix expression.
toNixExpr :: PackageName -> ResolvedDependency -> NExpr
toNixExpr name (Resolved (unpackPSC -> semver)) = toDepExpr name semver
toNixExpr name (Broken reason) = "brokenPackage" @@ mkNonRecSet
  ["name" $= mkStr (tshow name), "reason" $= mkStr (tshow reason)]

-- | Write a nix expression pretty-printed to a file.
writeNix :: MonadIO io => FilePath -> NExpr -> io ()
writeNix path = writeFileUtf8 path . (<> "\n") . tshow . prettyNix

-- | Gets the .nix filename of a semver. E.g. (0, 1, 2) -> 0.1.2.nix
toDotNix :: SemVer -> FilePath
toDotNix v = fromText $ tshow v <> ".nix"

-- | Get the .nix filename relative to the nodePackages folder of a package.
toRelPath :: PackageName -> SemVer -> FilePath
toRelPath (PackageName name mNamespace) version = do
  let subPath = fromText name </> toDotNix version -- E.g. "foo/1.2.3.nix"
  case mNamespace of
    -- Simple case: package 'foo@1.2.3' -> './foo/1.2.3.nix'
    Nothing -> subPath
    -- Namespaced: package '@foo/bar@1.2.3' -> './@foo/bar/1.2.3.nix'
    Just nspace -> fromText ("/@" <> nspace) </> subPath

-- | Converts distinfo into a nix fetchurl call.
distInfoToNix :: Maybe Name -- `Just` if we are fetching from a namespace.
              -> Maybe DistInfo -> NExpr
distInfoToNix _ Nothing = Nix.mkPath False "./."
distInfoToNix maybeNamespace (Just DistInfo{..}) = do
  let fetchurl = case maybeNamespace of
        Nothing -> "pkgs" !. "fetchurl"
        Just _ -> "fetchUrlNamespaced"
      (algo, hash) = case diShasum of
        SHA1 hash' -> ("sha1", hash')
        SHA256 hash' -> ("sha256", hash')
      authBinding = case maybeNamespace of
        Nothing -> []
        Just namespace -> [bindTo "namespace" (mkStr namespace)]
      bindings = ["url" $= mkStr diUrl, algo $= mkStr hash] <> authBinding
  fetchurl @@ mkNonRecSet bindings

-- | Converts package meta to a nix expression, if it exists.
metaToNix :: PackageMeta -> Maybe NExpr
metaToNix PackageMeta{..} = do
  let
    grab name = maybe [] (\s -> [name $= mkStr s])
    homepage = grab "homepage" (map uriToText pmHomepage)
    description = grab "description" pmDescription
    author = grab "author" pmAuthor
    keywords = case pmKeywords of
      ks | null ks -> []
         | otherwise -> ["keywords" $= mkList (toList (map mkStr ks))]
    stdenvPlatforms = mkDots "pkgs" ["stdenv", "lib", "platforms"]
    platforms = case map nodePlatformToText $ toList pmPlatforms of
      [] -> []
      ps -> singleton $ "platforms" $= case ps of
        -- For a single one, just do pkgs.stdenv.lib.platforms.<platform>
        [p] -> stdenvPlatforms !. p
        -- For multiples, use the `with` syntax, and since each is a
        -- list, join with the concatenation operator.
        (p:ps) -> mkWith stdenvPlatforms $ foldl' ($++) (mkSym p) (mkSym <$> ps)
  case homepage <> description <> keywords <> author <> platforms of
    [] -> Nothing
    bindings -> Just $ mkNonRecSet bindings

-- | Returns true if any of the resolved package's dependencies were broken.
hasBroken :: ResolvedPkg -> Bool
hasBroken ResolvedPkg{..} = case rpDevDependencies of
  Nothing -> any isBroken rpDependencies
  Just devDeps -> any isBroken rpDependencies || any isBroken devDeps
  where isBroken = \case {Broken _ -> True; _ -> False}

-- | Given all of the versions defined in a node packages folder, create a
-- default.nix which defines an object that calls out to all of those files.
--
-- Essentially, given a directory structure like this:
-- > foo/
-- >   0.1.2.nix
-- >   0.2.3.nix
-- > bar/
-- >   1.2.3.nix
-- > @mynamespace/
-- >   qux/
-- >     3.4.5.nix
-- >     default.nix
--
-- We would generate a nix file that looks like this:
--
-- > {callPackage}:
-- >
-- > {
-- >   foo_0-1-2 = callPackage ./foo/0.1.2.nix {};
-- >   foo_0-2-3 = callPackage ./foo/0.2.3.nix {};
-- >   foo = callPackage ./foo/0.2.3.nix {};
-- >   bar_1-2-3 = callPackage ./bar/1.2.3.nix {};
-- >   bar = callPackage ./bar/1.2.3.nix {};
-- >   "@mynamespace-qux_3-4-5" =
-- >       callPackage (./. + "/@mynamespace/qux/3.4.5.nix") {};
-- >   "@mynamespace-qux" =
-- >       callPackage (./. + "/@mynamespace/qux/3.4.5.nix") {};
-- > }
--
-- Interestingly, it doesn't matter what the packagemap actually contains. We
-- can derive all of the information we need (names and versions) from the keys
-- of the map.
packageMapToNix :: PackageMap a -> NExpr
packageMapToNix pMap = do
  let
    -- Create a parameter set with no defaults given.
    params = mkParamset $ [("callPackage", Nothing)]
    -- Create the function body as a single set which contains all of the
    -- packages in the set as keys, and a callPackage to their paths as values.
    toBindings :: (PackageName, [SemVer]) -> [Binding NExpr]
    toBindings (pkgName, []) = []
    -- Grab the latest version and store that under a selector without a
    -- version.
    toBindings (pkgName, (latest:vs)) = binding : bindings  where
      binding = toSelectorNoVersion pkgName `NamedVar` call latest
      -- Convert render the name and version to a nix path. It might
      -- contain an '@' sign, in which case we'll need to use a trick
      -- to get it into a valid path.
      path version = case T.find (== '@') textPath of
        -- There is no '@' in the path. Just make a path nix expression.
        Nothing -> mkPath renderedPath
        -- There is an '@'. Then use a path addition; i.e. use the syntax
        --    ./. + "/this/@path"
        Just _ -> mkPath "." $+ mkStr textPath
        where renderedPath = toRelPath pkgName version
              textPath = pathToText renderedPath
      -- Equiv. to `callPackage path {}`
      call v = "callPackage" @@ path v @@ mkNonRecSet []
      toBinding :: SemVer -> Binding NExpr
      toBinding version = toSelector pkgName version `NamedVar` call version
      bindings :: [Binding NExpr]
      bindings = map toBinding (latest:vs)
    sortedPackages :: [(PackageName, [SemVer])]
    sortedPackages = do
      M.toList $ M.map (map fst . M.toDescList) $ psToMap pMap
    bindings :: [Binding NExpr]
    bindings = concatMap toBindings sortedPackages
  mkFunction params $ mkNonRecSet bindings

-- | Converts a resolved package object into a nix expression. The expresion
-- will be a function where the arguments are its dependencies, and its result
-- is a call to `buildNodePackage`.
resolvedPkgToNix :: ResolvedPkg -> NExpr
resolvedPkgToNix rPkg@ResolvedPkg{..} = mkFunction funcParams body
  where
    ---------------------------------------------------------------------------
    -- Circular dependency resolution
    --
    -- This is pretty gnarly but for now just deal with it...
    -- Step 1: throw out the "broken" packages from the dependencies.
    withoutBrokens = H.fromList $ go (H.toList rpDependencies)
      where go [] = []
            go ((_, Broken _):rest) = go rest
            go ((k, Resolved v):rest) = (k, v):go rest
    -- Step 2: separate the dependencies into circular and non-circular.
    (noncircDepMap, circDepMap) = sepCircularMap withoutBrokens
    -- Step 3: create lists of nix expressions for the circular and
    -- non-circular dependencies.
    deps = map (uncurry toDepExpr) $ H.toList noncircDepMap
    circDeps = flip map (H.toList circDepMap) $ \(name, CircularSemVer ver) ->
                 toDepExpr name ver
    ---------------------------------------------------------------------------
    optDeps = map (uncurry toNixExpr) $ H.toList rpOptionalDependencies
    -- Same for dev dependencies.
    devDeps = map (uncurry toNixExpr) . H.toList <$> rpDevDependencies
    -- List of arguments that these functions will take.
    funcParams' = catMaybes [
      Just "pkgs",
      Just "buildNodePackage",
      Just "nodePackages",
      -- If the package has any broken dependencies, we will need to include
      -- this function.
      maybeIf (hasBroken rPkg) "brokenPackage",
      -- If the package has a namespace then it will need to set headers
      -- when fetching. So add that function as a dependency.
      maybeIf (isNamespaced rpName) "fetchUrlNamespaced",
      maybeIf (isNamespaced rpName) "namespaceTokens",
      -- If any of the package's dependencies have namespaces, they will appear
      -- in the `namespaces` set, so we'll need that as a dependency.
      maybeIf (hasNamespacedDependency rPkg) "namespaces"
      ]
    -- None of these have defaults, so put them into pairs with Nothing.
    funcParams = mkParamset $ map (\x -> (x, Nothing)) funcParams'
    -- Wrap an list expression in a `with nodePackages;` syntax if non-empty.
    withNodePackages noneIfEmpty list = case list of
      [] -> if noneIfEmpty then Nothing else Just $ mkList []
      _ -> Just $ mkWith "nodePackages" $ mkList list
    devDepBinding = case devDeps of
      Nothing -> Nothing
      Just ddeps -> bindTo "devDependencies" <$> withNodePackages False ddeps
    PackageName name namespace = rpName
    args = mkNonRecSet $ catMaybes [
      Just $ "name" $= mkStr name,
      Just $ "version" $= (mkStr $ tshow rpVersion),
      Just $ "src" $= distInfoToNix (pnNamespace rpName) rpDistInfo,
      bindTo "namespace" <$> map mkStr namespace,
      bindTo "deps" <$> withNodePackages False deps,
      bindTo "circularDependencies" <$> withNodePackages True circDeps,
      bindTo "optionalDependencies" <$> withNodePackages True optDeps,
      devDepBinding,
      maybeIf (hasBroken rPkg) ("isBroken" $= mkBool True),
      bindTo "meta" <$> metaToNix rpMeta
      ]
    body = "buildNodePackage" @@ args

-- | Convenience function to generate an `import /path {args}` expression.
importWith :: Bool -- ^ True if the path is from the env, e.g. <nixpkgs>
           -> FilePath -- ^ Path to import
           -> [Binding NExpr] -- ^ Arguments to pass
           -> NExpr -- ^ The resulting nix expression
importWith isEnv path args = do
  "import" @@ Nix.mkPath isEnv (pathToString path) @@ mkNonRecSet args

-- | We use this a few times: `import <nixpkgs> {}`
importNixpkgs :: NExpr
importNixpkgs = importWith True "nixpkgs" []

-- | The default version of nodejs we are using; this should correspond
-- to a key in the nixpkgs set we're importing.
defaultNodeJS :: Text
defaultNodeJS = "nodejs-8_x"

-- | Also used a few times, these are the top-level params to the generated
-- default.nix files.
defaultParams :: Params NExpr
defaultParams = do
  mkParamset [("pkgs", Just importNixpkgs),
              ("nodejs", Just $ "pkgs" !. defaultNodeJS)]

-- | When passing through arguments, we inherit these things.
defaultInherits :: [Binding NExpr]
defaultInherits = [inherit ["pkgs", "nodejs"]]

-- | The name of the subfolder within the output directory that
-- contains node packages.
nodePackagesDir :: FilePath
nodePackagesDir = "nodePackages"

bindRootPath :: Binding NExpr
bindRootPath = "nodePackagesPath" $= mkPath ("./" </> nodePackagesDir)

-- | The root-level default.nix file.
rootDefaultNix :: NExpr
rootDefaultNix = mkFunction defaultParams body where
  lets = [
      "mkNodeLib" $= importWith False "./nodeLib" ["self" $= "mkNodeLib"]
    , "nodeLib" $= ("mkNodeLib" @@ mkNonRecSet defaultInherits)
    ]
  genPackages = "nodeLib" !. "generatePackages"
  body = mkLets lets $ genPackages @@ (mkNonRecSet [bindRootPath])

-- | Create a `default.nix` file for a particular package.json; this simply
-- imports the package as defined in the given path, and calls into it.
packageJsonDefaultNix :: FilePath -- ^ Path to the output directory.
                      -> NExpr
packageJsonDefaultNix outputPath = do
  let
    libBind = "lib" $= importWith False outputPath defaultInherits
    callPkg = "lib" !. "callPackage"
    call = callPkg @@ mkPath "project.nix" @@ mkNonRecSet []
  mkFunction defaultParams $ mkLets [libBind] call

bindingsToMap :: [Binding t] -> Record t
bindingsToMap = foldl' step mempty where
  step record binding = case binding of
    NamedVar [StaticKey key] obj -> H.insert key obj record
    _ -> record
