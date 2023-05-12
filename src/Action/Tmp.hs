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
import Data.Functor ((<&>), ($>))
import Data.List (intersperse)
import Data.Either (fromRight, lefts, rights)
import Data.List.NonEmpty (NonEmpty( (:|) ))
import qualified Data.List.NonEmpty as NE
import qualified Action.Tmp.ParseTag as P
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import Data.Maybe (fromMaybe, isJust, maybeToList, catMaybes)
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
import Data.Bifunctor (bimap, first)
import Data.Word (Word8)
import qualified Data.Map.Strict as Map
import qualified Data.Tuple
import qualified Data.Text.IO
import Data.Functor.Identity (Identity (Identity), runIdentity)

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

parseFile ::
  T.Text
  -> FilePath
  -> IO (Either (MP.ParseErrorBundle [Tag T.Text] Void) [Res])
parseFile modName fn = do
  content <- Data.Text.IO.readFile fn
  let tags = Html.parseTags content
  pure $ MP.parse (newParser modName) "lol" tags

-- | Fully qualified identifier
newtype Identifier str = Identifier { unIdentifier :: str }
  deriving (Eq, Show, Ord, Monoid, Semigroup)

renderIdentifier :: Identifier T.Text -> T.Text
renderIdentifier = T.strip . unIdentifier

data ParenExpr
  = Paren_Start -- (
  | Paren_End -- )
    deriving (Eq, Show, Ord)

renderParenExpr :: ParenExpr -> T.Text
renderParenExpr = \case
  Paren_Start -> "("
  Paren_End -> ")"

data ExprToken
  = ExprToken_Paren [ParenExpr]
  | ExprToken_Identifier (Identifier T.Text)
    deriving (Eq, Show, Ord)

renderExprTokens
  :: [ExprToken] -- TODO: NonEmpty or newtype
  -> T.Text
renderExprTokens =
  T.strip . mconcat . intersperse " " . map renderToken
  where
    renderToken = \case
      ExprToken_Paren parenList -> mconcat $ map renderParenExpr parenList
      ExprToken_Identifier ident -> renderIdentifier ident

-- | A function that takes zero or more arguments
--    (zero argument function is a value)
data Fun str = Fun
  { funName :: T.Text
  , funArg :: [ExprToken] -- TODO: NonEmpty
  , funRet :: [[ExprToken]] -- TODO: [NonEmpty]
  } deriving (Eq, Show, Ord)

renderFun
  :: Fun T.Text
  -> T.Text
renderFun fun =
  T.unwords $
      funName fun
    : "::"
    : intersperse "->" ids
  where
    ids :: [T.Text]
    ids = map renderExprTokens $ funArg fun : funRet fun

match
  :: (MP.MonadParsec e s m, MP.Token s ~ Tag T.Text, MP.Tokens s ~ [Tag T.Text])
  => (Maybe (Tag T.Text) -> Maybe Bool)
  -> m ()
match =
  void . P.satisfy . match'
  where
    match' :: (Maybe a -> Maybe Bool) -> a -> Bool
    match' f = \i -> fromMaybe False (f $ Just i)

pName
  :: T.Text
  -> MP.ParsecT Void [Tag T.Text] Identity Res
pName modName = do
  MP.dbg "p_open" $ match $ \mi -> do
    TagOpen "p" attrs <- mi
    pure $ lookup "class" attrs == Just "src"
  MP.dbg "a_open" $ match $ \mi -> do
    TagOpen "a" attrs <- mi
    pure $ lookup "class" attrs == Just "def"
  name <- MP.dbg "name" $ P.tagText >>= \(TagText name) -> pure $ unlines [T.unpack name] `trace` name
  _ <- MP.dbg "a_close" $ P.tagClose "a"
  void pHasType
  (firstArg, mNextIdentPrefix) <- MP.dbg "arg1" (parseIdentifier Nothing) >>= \case
    Left a -> pure (a, Nothing)
    Right a -> pure a
  () <- pure $ show firstArg `trace` ()
  -- Zero or more args
  let parseManyArgs = flip fix (mNextIdentPrefix, [], 2) $ \go (mNextIdentPrefix', accum, count) ->
        MP.dbg ("arg" <> show count) (parseIdentifier mNextIdentPrefix') >>= \case
          Left res -> pure $ reverse $ res : accum
          Right (res, mNextIdentPrefix'') -> go (mNextIdentPrefix'', res : accum, count + 1)
  remArgs <- MP.dbg "remArgs" parseManyArgs
  pure $ Fun
    { funName = name
    , funArg = firstArg
    , funRet = remArgs
    }
  where
    parseIdentifier mPrefix = do
      (idFragments, end) <- MP.someTill_
        (MP.dbg "identifierOrParens" $ parseIdentifierFragment <|> parseParensFromTag)
        ((Right <$> pArrow) <|> (Left <$> pEndFunctionSignature))
      let parseMaybeParensText
            :: Maybe T.Text
            -> MP.ParsecT Void [Tag T.Text] Identity ExprToken
          parseMaybeParensText mParensText =
              either (parseFail . (: []) . show) pure $
                runIdentity $ MP.runParserT parseParens (T.unpack modName) (fromMaybe "" mParensText)
          eRemainder :: MP.ParsecT Void [Tag T.Text] Identity (Maybe ExprToken, Maybe T.Text)
          eRemainder = case end of
            Left () -> pure (Nothing, Nothing) -- There's nothing left
            Right (mBefore, mAfter) -> do -- There's more left
              -- TODO: what if it's a type variable?
              res <- parseMaybeParensText mBefore
              pure (Just res, mAfter)
      (mRemainder, mNextIdentPrefix) <- eRemainder -- "mNextIdentPrefix" is a terrible name for paren(s) after an arrow, which need to be included as a prefix of the next identifier we parse using 'parseIdentifier'
      prevExtra <- parseMaybeParensText mPrefix
      -- TODO: don't include "prevExtra" equal to "ExprToken_Paren []""
      let result = prevExtra : idFragments ++ maybeToList mRemainder
      pure $ case end of
        Left () -> Left result -- no more left
        Right _ -> Right (result, mNextIdentPrefix) -- more left

    pHasType =
      MP.dbg "has type" $ match $ \mi -> do
        TagText " :: " <- mi
        pure True

    pArrow
      :: MP.ParsecT Void [Tag T.Text] m
          (Maybe T.Text, Maybe T.Text) -- (text before arrow, text after arrow)
    pArrow =
      MP.dbg "arrow" $ do
        let testTag (TagText t) =
              let arrow = " -> "
                  toResult t' = if T.null t' then Nothing else Just t'
                  mResult =
                    case arrow `T.breakOn` t of
                      (_, "") -> Nothing -- text does not contain an arrow
                      (prefix, suffix) ->
                        Just
                          ( toResult prefix
                          , toResult $ fromMaybe (error "BUG: pArrow") $ T.stripPrefix arrow suffix
                          )
              in mResult
            testTag _ = Nothing
        MP.token testTag mempty

    parseIdentifierFragment :: MP.ParsecT Void [Tag T.Text] Identity ExprToken
    parseIdentifierFragment = MP.dbg "parseIdentifierFragment" $ do
      moduleName <- do
        let testTag :: Tag T.Text -> Maybe T.Text
            testTag (TagOpen "a" attrs) = lookup "title" attrs
            testTag _ = Nothing
        MP.token testTag mempty
      typeName <- do
        let testTag (TagText typeName) = Just typeName
            testTag _ = Nothing
        MP.token testTag mempty
      P.tagClose "a"
      pure $ ExprToken_Identifier $ Identifier $ moduleName <> "." <> typeName

    parseParensFromTag :: MP.ParsecT Void [Tag T.Text] Identity ExprToken
    parseParensFromTag = MP.dbg "parseParensFromTag" $ do
      parens <- MP.token
        (\(token :: Tag T.Text) -> case token of { TagText t -> Just t; _ -> Nothing })
        (Set.fromList [MP.Label $ NE.fromList "text"])
      let eRes :: Identity (Either String ExprToken)
          eRes = fmap (first MP.errorBundlePretty) $
            MP.runParserT parseParens (T.unpack modName) parens
      either (parseFail . (: [])) pure (runIdentity eRes)

    parseParens :: MP.ParsecT Void T.Text Identity ExprToken
    parseParens = ExprToken_Paren <$> parenFromText

    -- do
    --   Optional:  <a ... class="link">Source</a>
    --   Mandatory: <a ... class="selflink">#</a>
    pEndFunctionSignature = MP.dbg "entryEnd" $ void $ do
      -- Optional "Source" link
      MP.optional $ do
        match $ \mi -> do
          TagOpen "a" attrs <- mi
          pure $ lookup "class" attrs == Just "link"
        match $ \mi -> do
          TagText "Source" <- mi
          pure True
        P.tagClose "a"
      -- Mandatory "#" link
      match $ \mi -> do
        TagOpen "a" attrs <- mi
        pure $ lookup "class" attrs == Just "selflink"
      match $ \mi -> do
        TagText "#" <- mi
        pure True
      P.tagClose "a"

parenFromText
  :: MP.ParsecT Void T.Text Identity [ParenExpr]
parenFromText = MP.dbg "parenFromText" $ fmap catMaybes $
  MP.manyTill ((Just <$> parenToken) <|> (MP.space $> Nothing)) MP.eof
  where
    parenToken =
      MP.dbg "parenFromText.token" $ MP.token
        (`Map.lookup` charToParenExpr)
        (Set.fromList [MP.Label $ NE.fromList "'(' or ')'"])

    charToParenExpr :: Map.Map Char ParenExpr
    charToParenExpr = mapSwap parenExprToText
    parenExprToText :: Map.Map ParenExpr Char
    parenExprToText = Map.fromList
      [ (Paren_Start, '(')
      , (Paren_End, ')')
      ]

    mapSwap :: Ord b => Map.Map a b -> Map.Map b a
    mapSwap = Map.fromList . map Data.Tuple.swap . Map.assocs

type Res = Fun T.Text

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
          pure $ reverse accum

-- ### Util

decodeUtf8 = TE.decodeUtf8With TEE.lenientDecode

removeSpaces :: T.Text -> T.Text
removeSpaces =
  T.filter (`elem` T.unpack " ")

parseFail
  :: MP.MonadParsec Void s m
  => [String]
  -> m a
parseFail =
  MP.parseError . mkFancyError
    where
    mkFancyError errLst =
      MP.FancyError 0 $ Set.fromList (map MP.ErrorFail errLst :: [MP.ErrorFancy Void])

tshow :: Show a => a -> T.Text
tshow = T.pack . show
