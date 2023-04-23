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
import Control.Monad (forM_, unless)
import qualified Text.HTML.TagSoup as Html
import Text.HTML.TagSoup (Tag(..))
import Text.Show.Pretty
import qualified Data.String
import Data.Functor ((<&>))
import Data.List (intersperse)
import Data.Either (fromRight, lefts, rights)

import qualified Action.Tmp.ParseTag as P
import qualified Text.Megaparsec as MP
import Data.Maybe (fromMaybe)
import Control.Monad.Fix (mfix)
import Data.Function (fix)
import qualified Data.Set as Set
import qualified Text.Megaparsec.Debug as MP
import Data.Void (Void)

main fn = do
  content <- C8.readFile fn
  let cLines = C8.lines content
  forM_ cLines $ \cLine -> do
    let txtLine = T.pack . C8.unpack $ cLine
    either
      (\err -> IO.hPutStrLn IO.stderr $ "Failed to decode line '" <> T.unpack txtLine <> "' error: " <> err)
      T.putStrLn
      (HTML.decode txtLine)
    -- TODO: print warning on Left

readHtml modName fn = do
  content <- BS.readFile fn
  let tags = Html.parseTags content
  let parsedTagsE = parseTags tags
      parsedTags = fromRight (error ":(") parsedTagsE
  forM_ parsedTags (BS.putStrLn . mconcat . intersperse " " . renderFun)
  where
    parseTags :: [Tag BS.ByteString] -> Either String [Fun BS.ByteString]
    parseTags tags =
      let go :: ([Tag BS.ByteString], [Fun BS.ByteString])
              -> Either String ([Tag BS.ByteString], [Fun BS.ByteString])
          go ([], accum) = Right ([], accum)
          go (state@(_ : stateRest), accum) =
            case parseFun modName state of
              Left err -> Left err
              Right Nothing -> go (stateRest, accum)
              Right (Just (fun, stateRest')) -> go (stateRest', fun : accum)
      in snd <$> go (tags, [])

parseFun
  :: (Eq str, Data.String.IsString str, Semigroup str, Show str)
  => str -- ^ Module name
  -> [Tag str] -- ^ Parser input
  -> Either String (Maybe (Fun str, [Tag str]))
  -- ^ Right: on successful parse a Fun and the remainder of the input
parseFun modName = \case
  [] -> Right Nothing
  TagOpen "p" [ ( "class" , "src" ) ]
    : TagOpen "a" tagsName -- [ ( "id" , "v:fromLazyText" ) , ( "class" , "def" ) ]
    : TagText name
    : TagClose "a"
    : TagText " :: "
    : TagOpen
        "a"
        tagsSrc
        -- [ ( "href"
        --   , "file:///nix/store/hs8dbwgs3mxj6qnl3zxbb8rp22722fv6-ghc-8.6.5-doc/share/doc/ghc/html/libraries/text-1.2.3.1/Data-Text-Lazy.html#t:Text"
        --   )
        -- , ( "title" , "Data.Text.Lazy" )
        -- ]
    : TagText _ -- "Text"
    : TagClose "a"
    : TagText " -> "
    : TagOpen
        "a"
        tagsDst
        -- [ ( "href" , "Blaze-ByteString-Builder.html#t:Builder" )
        -- , ( "title" , "Blaze.ByteString.Builder" )
        -- ]
    : TagText _ -- "Builder"
    : TagClose "a"
    : xs ->
        Fun (modName <> "." <> name)
          <$> ((:[]) <$> lookupE "title" tagsSrc)
          <*> ((:[]) <$> lookupE "title" tagsDst)
            <&> \fun -> Just (fun, xs)
  _ -> Right Nothing
  where
    lookupE name tags = maybe (Left $ show name <> " not found in " <> show tags) Right (lookup name tags)

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

p
  :: (P.MonadParsec e s m, e ~ Void, P.StringLike str, P.Token s ~ Tag str, Show str, Semigroup str)
  => str
  -> m (Fun str)
p modName = do
  P.tagOpen "p"
  P.tagOpen "a"
  name <- P.tagText >>= \(TagText name) -> pure name
  P.tagClose "a"
  match $ \mi -> do
    TagText t <- mi
    pure $ t == " :: "
  tagsSrc <- flip fix [] $ \loop accum ->
    P.anyTag >>= \case
      TagText " -> " ->
        pure accum
      other ->
        loop (other : accum)
  src <- extractSomething tagsSrc
  tagsDst <- flip fix [] $ \loop accum ->
    P.anyTag >>= \case
      TagOpen "a" attrs | lookup "class" attrs == Just "link" ->
        pure accum
      other ->
        loop (other : accum)
  dst <- extractSomething tagsDst
  pure $ Fun (modName <> "." <> name) src dst

  where
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

-- <a href="src/GHC-IO-Encoding-UTF16.html#utf16_decode" class="link">Source</a>

parse modName =
  MP.parse (MP.dbg "" p') "lol"
  where
    p' = MP.skipManyTill P.anyTag debugP -- (p modName)

    debugP = do
      -- P.tagOpen "!DOCTYPE"
      -- P.tagOpen "html"
      MP.dbg "1" $ P.tagOpen "head"
      MP.dbg "2" $ P.tagOpen "meta"
      MP.dbg "3" $ P.tagClose "meta"

parseFile ::
  p
  -> FilePath
  -> IO
      (Either
          (MP.ParseErrorBundle [Tag BS.ByteString] Void) (Tag BS.ByteString))
parseFile modName fn = do
  content <- BS.readFile fn
  let tags = Html.parseTags content
  pPrint tags
  pure $ parse modName tags

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

input =
    [ TagOpen "p" [ ( "class" , "src" ) ]
    , TagOpen "a" [ ( "id" , "v:fromLazyText" ) , ( "class" , "def" ) ]
    , TagText "someName"
    , TagClose "a"
      -- , TagClose "b"
    , TagText " :: "
    , TagOpen
        "a"
        [ ( "href"
          , "file:///nix/store/hs8dbwgs3mxj6qnl3zxbb8rp22722fv6-ghc-8.6.5-doc/share/doc/ghc/html/libraries/text-1.2.3.1/Data-Text-Lazy.html#t:Text"
          )
        , ( "title" , "Data.Text.Lazy" )
        ]
    , TagText "Text"
    , TagClose "a"
    , TagText " -> "
    , TagOpen
        "a"
        [ ( "href" , "Blaze-ByteString-Builder.html#t:Builder" )
        , ( "title" , "Blaze.ByteString.Builder" )
        ]
    , TagText "Builder"
    , TagClose "a"
    , TagText "blah"
    ]