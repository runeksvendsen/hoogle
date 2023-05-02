{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Action.Tmp where

import qualified Text.HTMLEntity as HTML
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.IO as IO
import Control.Monad (forM_, unless, guard, void, when)
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
    (\resLst -> do
      putStrLn "Success! Results:"
      forM_ resLst $ \res ->
        when (isFunction res) $
          print res
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
  pHasType
  -- mRhsLhs <- MP.between pHasType (MP.dbg "end" pEndFunctionSignature) $ do
  (lhs, end) <- MP.someTill_
    (traceAs "anyTag BEFORE arrow" $ parseIdentifier <|> parseParens)
    ((Left <$> pArrow) <|> (Right <$> (MP.dbg "lhs end" $ MP.try pEndFunctionSignature)))
  (mRhs, remLhs) <- case end of
    Right _ -> pure (Nothing, "")
    Left remLhs -> do
      res <- MP.someTill (traceAs "anyTag AFTER arrow" P.anyTag) (MP.dbg "rhs end" $ MP.try pEndFunctionSignature)
      pure (Just res, remLhs)
    -- pure
  pure (name, (lhs ++ [Identifier remLhs], mRhs))
  where
    traceAs name action = MP.dbg name action

    pHasType =
      MP.dbg "has type" $ match $ \mi -> do
        TagText " :: " <- mi
        pure True

    pArrow =
      MP.dbg "arrow" $ do
        let testTag (TagText t) = " -> " `BS.stripSuffix` t
            testTag _ = Nothing
        MP.token testTag mempty

    parseIdentifier = do
      moduleName <- do
        let testTag (TagOpen "a" attrs) = lookup "title" attrs
            testTag _ = Nothing
        MP.token testTag mempty
      typeName <- do
        let testTag (TagText typeName) = Just typeName
            testTag _ = Nothing
        MP.token testTag mempty
      P.tagClose "a"
      pure $ Identifier $ moduleName <> "." <> typeName

    parseParens = do
      let testTag (TagText parens) = Just $ Identifier parens
          testTag _ = Nothing
      MP.token testTag mempty

    pEndFunctionSignature = void $
      match $ \mi -> do
        TagOpen "a" attrs <- mi
        pure $ lookup "class" attrs == Just "link"

-- | Fully qualified identifier
newtype Identifier str = Identifier str
  deriving (Eq, Show, Ord, Monoid, Semigroup)

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

type Res = (BS.ByteString, ([Identifier BS.ByteString], Maybe [Tag BS.ByteString]))
data Result str a = UselessTag (Tag str) | Good Res | EOF

isFunction :: Res -> Bool
isFunction = isJust . snd . snd

newParser modName =
  go []
  where
    go accum = do
      r <- parser
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
