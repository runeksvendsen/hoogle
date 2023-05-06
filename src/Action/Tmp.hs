{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import qualified Text.StringLike as SL
import Data.Bifunctor (bimap)

-- | Fully qualified identifier
newtype Identifier str = Identifier { unIdentifier :: str }
  deriving (Eq, Show, Ord, Monoid, Semigroup)

renderIdentifier :: Identifier BS.ByteString -> T.Text
renderIdentifier = T.strip . decodeUtf8 . unIdentifier

-- | A function that takes zero or more arguments
--    (zero argument function is a value)
data Fun str = Fun
  { funName :: T.Text
  , funArg :: Identifier str
  , funRet :: [Identifier  str]
  } deriving (Eq, Show)

renderFun
  :: Fun BS.ByteString
  -> T.Text
renderFun fun =
  T.unwords $
      funName fun
    : "::"
    : intersperse "->" ids
  where
    ids :: [T.Text]
    ids = map renderIdentifier $ funArg fun : funRet fun

testParseHtml = do
  eRes <- parseFile
    "Data.Text.Internal.Builder"
    "/nix/store/hs8dbwgs3mxj6qnl3zxbb8rp22722fv6-ghc-8.6.5-doc/share/doc/ghc/html/libraries/base-4.12.0.0/GHC-IO-Encoding-UTF16.html"
  either
    print -- TODO: use MP.errorBundlePretty
    (\resLst -> do
      putStrLn "Success! Results:"
      forM_ resLst $ \fun -> do
          TIO.putStrLn $ renderFun fun
    )
    eRes

match
  :: (P.StringLike str, MP.MonadParsec e s m, MP.Token s ~ Tag str)
  => (Maybe (Tag str) -> Maybe Bool)
  -> m (Tag str)
match =
  P.satisfy . match'
  where
    match' :: (Maybe a -> Maybe Bool) -> a -> Bool
    match' f = \i -> fromMaybe False (f $ Just i)

pName
  :: forall s m.
     ( MP.VisualStream s
     , MP.Token s ~ Tag BS.ByteString
     )
  => BS.ByteString
  -> MP.ParsecT Void s m Res
pName modName = do
  MP.dbg "p_open" $ match $ \mi -> do
    TagOpen "p" attrs <- mi
    pure $ lookup "class" attrs == Just "src"
  MP.dbg "a_open" $ match $ \mi -> do
    TagOpen "a" attrs <- mi
    pure $ lookup "class" attrs == Just "def"
  name <- MP.dbg "name" $ P.tagText >>= \(TagText name) -> pure $ unlines [show name] `trace` name
  MP.dbg "a_close" $ P.tagClose "a"
  void pHasType
  firstArg <- MP.dbg "firstArg" parseIdentifier >>= \case
    Left a -> pure a
    Right a -> pure a
  -- Zero or more args
  let parseManyArgs = flip fix [] $ \go accum ->
        parseIdentifier >>= \case
          Left res -> pure $ reverse $ res : accum
          Right res -> go $ res : accum
  remArgs <- MP.dbg "remArgs" parseManyArgs
  pure $ Fun
    { funName = decodeUtf8 $ modName <> "." <> name
    , funArg = firstArg
    , funRet = remArgs
    }
  where
    -- parseFail = MP.parseError . mkFancyError

    parseIdentifier = do
      (idFragments, end) <- MP.someTill_
        (traceAs "identifier" $ parseIdentifierFragment <|> parseParens)
        ((Right <$> pArrow) <|> (MP.try $ Left <$> (MP.dbg "entry end" pEndFunctionSignature)))
      let mkRes :: BS.ByteString -> Identifier BS.ByteString
          mkRes rem' = Identifier $ mconcat $ idFragments ++ [rem]
          rem = case end of
            Left () -> ""
            Right remLhs -> remLhs
      pure $ bimap (const $ mkRes "") mkRes end

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

    parseIdentifierFragment :: MP.ParsecT Void s m BS.ByteString
    parseIdentifierFragment = do
      moduleName <- do
        let testTag (TagOpen "a" attrs) = lookup "title" attrs
            testTag _ = Nothing
        MP.token testTag mempty
      typeName <- do
        let testTag (TagText typeName) = Just typeName
            testTag _ = Nothing
        MP.token testTag mempty
      P.tagClose "a"
      pure $ moduleName <> "." <> typeName

    parseParens :: MP.ParsecT Void s m BS.ByteString
    parseParens = do
      let testTag (TagText parens) = Just $ removeSpaces parens
          testTag _ = Nothing
      MP.token testTag mempty

    pEndFunctionSignature = void $
      match $ \mi -> do
        TagOpen "a" attrs <- mi
        pure $ lookup "class" attrs == Just "link"

    mkFancyError errLst =
      MP.FancyError 0 $ Set.fromList (map MP.ErrorFail errLst :: [MP.ErrorFancy Void])

type Res = Fun BS.ByteString

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

newParser modName =
  go []
  where
    parser = (Good <$> MP.try (pName modName)) <|> (UselessTag <$> P.anyTag) <|> (EOF <$ MP.eof)

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

removeSpaces :: BS.ByteString -> BS.ByteString
removeSpaces =
  BS.filter (/= space)
  where
    [space] = BS.unpack " " -- TODO: is there a better way to construct a ' ' Word8?
