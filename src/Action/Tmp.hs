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
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO

main fn = testParseHtml

testParseHtml = do
  eRes <- parseFile
    "GHC.IO.Encoding.UTF16"
    "/nix/store/hs8dbwgs3mxj6qnl3zxbb8rp22722fv6-ghc-8.6.5-doc/share/doc/ghc/html/libraries/base-4.12.0.0/GHC-IO-Encoding-UTF16.html"
  either
    print -- TODO: use MP.errorBundlePretty
    (\resLst -> do
      putStrLn "Success! Results:"
      forM_ resLst $ \res -> do
        let mFun = resToFun res
        forM_ mFun $ \fun ->
          TIO.putStrLn . T.unwords $ renderFun fun
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

pName modName = do
  MP.dbg "p_open" $ match $ \mi -> do
    TagOpen "p" attrs <- mi
    pure $ lookup "class" attrs == Just "src"
  MP.dbg "a_open" $ match $ \mi -> do
    TagOpen "a" attrs <- mi
    pure $ lookup "class" attrs == Just "def"
  name <- MP.dbg "name" $ P.tagText >>= \(TagText name) -> pure $ unlines [show name] `trace` name
  MP.dbg "a_close" $ P.tagClose "a"
  pHasType
  (lhs, end) <- MP.someTill_
    (traceAs "LHS identifier" $ parseIdentifier <|> parseParens)
    ((Left <$> pArrow) <|> (Right <$> (MP.dbg "LHS end" $ MP.try pEndFunctionSignature)))
  (mRhs, remLhs) <- case end of
    Right _ -> pure (Nothing, "")
    Left remLhs -> do
      res <- MP.someTill
        (traceAs "RHS identifier" $ parseIdentifier <|> parseParens)
        (MP.dbg "RHS end" $ MP.try pEndFunctionSignature)
      pure (Just res, remLhs)
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
newtype Identifier str = Identifier { unIdentifier :: str }
  deriving (Eq, Show, Ord, Monoid, Semigroup)

type Res = (BS.ByteString, ([Identifier BS.ByteString], Maybe [Identifier BS.ByteString]))

resToFun :: Res -> Maybe (Fun T.Text)
resToFun (name, (lhs, mRhs)) =
  case mRhs of
    Nothing -> Nothing
    Just rhs -> Just $
      Fun
        { funName = decodeUtf8 name
        , funArg = [renderId lhs]
        , funRet = [renderId rhs]
        }
  where
    renderId :: [Identifier BS.ByteString] -> T.Text
    renderId = T.unwords . map (T.strip . decodeUtf8 . unIdentifier)

tmpTestIdList =
  [ Identifier "Data.IORef.IORef"
  , Identifier " ("
  , Identifier "GHC.Maybe.Maybe"
  , Identifier "GHC.IO.Encoding.Types.DecodeBuffer"
  , Identifier ")"
  ]

data Result str a
  = UselessTag (Tag str)
  | Good Res
  | EOF

isFunction :: Res -> Bool
isFunction = isJust . snd . snd

newParser modName =
  go []
  where
    parser = (Good <$> pName modName) <|> (UselessTag <$> P.anyTag) <|> (EOF <$ MP.eof)

    go accum = do
      r <- parser
      case r of
        Good res -> do
          go $ res : accum
        UselessTag _ ->
          go accum
        EOF ->
          pure accum

parseFile ::
  BS.ByteString
  -> FilePath
  -> IO (Either (MP.ParseErrorBundle [Tag BS.ByteString] Void) [Res])
parseFile modName fn = do
  content <- BS.readFile fn
  let tags = Html.parseTags content
  pure $ MP.parse (newParser modName) "lol" tags

-- ### Util

decodeUtf8 = TE.decodeUtf8With TEE.lenientDecode
