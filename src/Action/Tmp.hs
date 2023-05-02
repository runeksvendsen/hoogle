{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Action.Tmp where

import qualified Text.HTMLEntity as HTML
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.IO as IO
import Control.Monad (forM_, unless, guard, void)
import qualified Text.HTML.TagSoup as Html
import Text.HTML.TagSoup (Tag(..))
import Text.Show.Pretty
import qualified Data.String
import Data.Functor ((<&>))
import Data.List (intersperse)
import Data.Either (fromRight, lefts, rights)

import qualified Action.Tmp.ParseTag as P
import qualified Text.Megaparsec as MP
import Data.Maybe (fromMaybe, isJust)
import Control.Monad.Fix (mfix)
import Data.Function (fix)
import qualified Data.Set as Set
import qualified Text.Megaparsec.Debug as MP
import Data.Void (Void)
import Debug.Trace (trace)
import Control.Applicative ((<|>))

main fn = testParseHtml

testParseHtml = do
  eRes <- parseFile
    "GHC.IO.Encoding.UTF16"
    "/nix/store/hs8dbwgs3mxj6qnl3zxbb8rp22722fv6-ghc-8.6.5-doc/share/doc/ghc/html/libraries/base-4.12.0.0/GHC-IO-Encoding-UTF16.html"
  either
    print -- MP.errorBundlePretty
    (\res -> do
      putStrLn "Success! Results:"
      print res
      -- (BS.putStrLn . mconcat . intersperse " " . renderFun) res
    )
    eRes


data Fun str = Fun
  { funName :: str
  , funArg :: [str]
  , funRet :: [str]
  } deriving (Eq, Show)

renderFun :: (Data.String.IsString str, Monoid str) => Fun str -> [str]
renderFun fun = [funName fun, "::", mconcat $ funArg fun, "->", mconcat $ funRet fun]

match
  :: (P.StringLike str, MP.MonadParsec e s m, MP.Token s ~ Tag str)
  => (Maybe (Tag str) -> Maybe Bool)
  -> m (Tag str)
match =
  P.satisfy . match'
  where
    match' :: (Maybe a -> Maybe Bool) -> a -> Bool
    match' f = \i -> fromMaybe False (f $ Just i)

-- p :: (MP.VisualStream s, MP.Token s ~ Tag str, Show str, P.StringLike str, Semigroup str) =>
--   str -> MP.ParsecT Void s m (Fun str)
pName modName = do
  -- MP.dbg "p_open" $ P.tagOpen "p"
  MP.dbg "p_open" $ match $ \mi -> do
    TagOpen "p" attrs <- mi
    pure $ lookup "class" attrs == Just "src"
  -- MP.dbg "a_open" $ P.tagOpen "a"
  MP.dbg "a_open" $ match $ \mi -> do
    TagOpen "a" attrs <- mi
    pure $ lookup "class" attrs == Just "def"
  name <- MP.dbg "name" $ P.tagText >>= \(TagText name) -> pure $ unlines [show name] `trace` name
  MP.dbg "a_close" $ P.tagClose "a"
  MP.dbg "has type" $ match $ \mi -> do
    TagText t <- mi
    pure $ t == " :: "
  -- 1. TagOpen containing module name in the "title" attribute
  firstTypeModuleTag@(TagOpen "a" attrs) <- match $ \mi -> do -- TODO: write a combinator that can return what it matches on
    TagOpen "a" attrs <- mi
    pure $ isJust $ lookup "title" attrs
  let firstTypeModule = fromMaybe (error $ "BUG: firstTypeTag: " <> show firstTypeModuleTag) $ lookup "title" attrs
  -- 2. TagText containing the (unqualified) type name
  TagText firstTypeName <- match $ \mi -> do -- TODO: write a combinator that can return what it matches on
    TagText _ <- mi
    pure True
  -- 3. TagClose "a"
  P.tagClose "a"
  toEndOfSignature <- pEndFunctionSignature -- >>= \eos -> pure $ ("toEndOfSignature: " <> show eos) `trace` eos
  pure (name, firstTypeModule <> "." <> firstTypeName, toEndOfSignature)


pSrc name = do
  tagsSrc <- flip fix [] $ \loop accum -> do
    someTag <- P.anyTag >>= \tag' -> pure $ ("tagsSrc loop: " <> show tag') `trace` tag'
    case someTag of
      TagText " -> " -> do
        let res = reverse accum
        pure $ ("got src: " <> show res) `trace` reverse accum
      other ->
        loop (other : accum)
  src <- extractSomething tagsSrc >>= \src' -> pure $ ("extracted src: " <> show src') `trace` src'

  pure (name, src)

-- pEndFunctionSignature
--   :: (MP.MonadParsec e s m, P.StringLike str, MonadFail m, MP.Token s ~ Tag str, Show str, MP.VisualStream s) => m [Tag str]
pEndFunctionSignature = MP.dbg "pEndFunctionSignature" $
  MP.manyTill P.anyTag $ match $ \mi -> do
      TagOpen "a" attrs <- mi
      pure $ lookup "class" attrs == Just "link"

pRest modName name src = do
  tagsDst <- flip fix [] $ \loop accum ->
    MP.dbg ("tagsDst") $ P.anyTag >>= \case
      TagOpen "a" attrs | lookup "class" attrs == Just "link" ->
        pure $ reverse accum
      other ->
        ("tagsDst loop: " <> show other) `trace` loop (other : accum)
  dst <- extractSomething tagsDst
  pure $ ("got dst: " <> show dst) `trace` ()
  pure $ Fun (modName <> "." <> name) src dst

lookupE name tags = maybe (Left $ show name <> " not found in " <> show tags) Right (lookup name tags)

mkFancyError errLst =
  MP.FancyError 0 $ Set.fromList (map MP.ErrorFail errLst :: [MP.ErrorFancy Void])

extractSomething tags = do
  let res = tags <&> \case
        TagText t -> Right t
        TagOpen _ attrs -> lookupE "title" attrs
        other -> Left $ "Unexpected tag: " <> show other
      src = rights res
      errors = lefts res
  unless (null errors) $
    MP.parseError $ mkFancyError errors
  pure src

anyTagOrEof
  :: (P.StringLike str, MP.MonadParsec e s f, MP.Token s ~ Tag str)
  => f (Either () (Tag str))
anyTagOrEof = (Right <$> P.anyTag) <|> (Left <$> MP.eof)

type Res = (BS.ByteString, BS.ByteString, [Tag BS.ByteString])
data Result str a = UselessTag (Tag str) | Good Res | EOF

newParser modName =
  go []
  where
    go accum = do
      r <- (Good <$> pName') <|> (UselessTag <$> P.anyTag) <|> (EOF <$ MP.eof)
      case r of
        Good res -> do
          go $ res : accum
        UselessTag _ ->
          go accum
        EOF ->
          pure accum

    parser = (Good <$> pName') <|> (UselessTag <$> P.anyTag) <|> (EOF <$ MP.eof)

    pName' = pName modName --  >>= \res -> pure $ ("pName: " <> show res) `trace` res

parseFile ::
  BS.ByteString
  -> FilePath
  -> IO (Either (MP.ParseErrorBundle [Tag BS.ByteString] Void) [Res])
parseFile modName fn = do
  content <- BS.readFile fn
  let tags = Html.parseTags content
  -- pPrint tags
  pure $ MP.parse (newParser modName) "lol" tags
