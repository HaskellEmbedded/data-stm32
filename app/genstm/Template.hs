{-# LANGUAGE OverloadedStrings #-}

module Template
  ( getTemplate
  , getTemplatesPath
  , template
  , template'
  , templateRaw
  , listCtx
  ) where

import Turtle
import Prelude hiding (log)

import Data.Map (Map)
import qualified Data.Map
import Text.Mustache
import Text.Mustache.Types
import qualified Data.HashMap.Strict      as HM

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import Data.List (intercalate)
import Data.List.Split (splitOn)

import Types (MonadGen)

template
  :: (Show a, ToMustache a)
  => a
  -> Text
  -> Text
  -> MonadGen ()
template context namespace tmpl = do
  et <- compileTemplate "someName" <$> getTemplate tmpl
  case et of
    Left e -> error $ "Cannot compile template " ++ show e
    Right t -> do
      let
          ns = "Ivory.BSP." <> namespace

          nsInit :: Text -> Text
          nsInit = T.intercalate "." . map T.pack . init . splitOn "." . T.unpack

          insertContext =
              HM.insert "modns" (String ns)
            . HM.insert "init_modns" (String $ nsInit ns)

          ctxval = toMustache context

          val = case ctxval of
            Object o -> Object $ insertContext o
            Null -> Object $ insertContext mempty
            x -> error $ "Absurd value when templating" ++ show x

          errorRes = checkedSubstituteValue t val

      case errorRes of
        ([], res) -> liftIO $ writeHS ns res
        (xs, _) -> error
          $ "Substitute produced errors while rendering"
          ++ " "
          ++ T.unpack tmpl
          ++ "\n"
          ++ show xs
          ++ "\n"
          ++ "Input was: \n"
          ++ show context
          ++ "Val was: \n"
          ++ show val

template'
  :: Text
  -> Text
  -> MonadGen ()
template' = template ()

templateRaw
  :: (Show a, ToMustache a)
  => a
  -> Text
  -> MonadGen Text
templateRaw context tmpl = do
  et <- compileTemplate "someName" <$> getTemplate tmpl
  case et of
    Left e -> error $ "Cannot compile template " ++ show e
    Right t -> do
      case checkedSubstitute t context of
        ([], res) -> pure res
        (xs, _) -> error
          $ "Substitute produced errors while rendering"
          ++ " "
          ++ T.unpack tmpl
          ++ "\n"
          ++ show xs
          ++ "\n"
          ++ "Input was: \n"
          ++ show context

listCtx :: [(Text,Text)] -> Map Text Text
listCtx = Data.Map.fromList

getTemplatesPath :: MonadGen Text
getTemplatesPath = do
  mPath <- liftIO $ need "TEMPLATES_PATH"
  case mPath of
    Nothing -> liftIO $ die "need TEMPLATES_PATH env var"
    Just p -> return p

-- Read template from file
getTemplate :: Text -> MonadGen Text
getTemplate x = do
  tPath <- getTemplatesPath
  -- log $ T.unpack $ "Loading template " <> x <> " from " <> tPath
  liftIO $ TIO.readFile
         $ fromString
         $ T.unpack
         $ tPath <> "/" <> x

-- writeHS "STM32.Bla" "content"
writeHS :: Text -> Text -> IO ()
writeHS namespace content = do
  here <- pwd
  cd "src"
  mktree tree
  cd tree
  TIO.writeFile fname content
  cd here
  where
    tree = fromString $ T.unpack $ T.replace "." "/" $ fst sp
    fname = fromString $ T.unpack $ T.concat [snd sp, ".hs"]
    sp = T.breakOnEnd "." namespace
