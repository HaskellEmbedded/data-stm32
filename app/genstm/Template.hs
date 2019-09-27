{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Template where

import Turtle
import Prelude hiding (log)
import Text.Hastache
import Text.Hastache.Context
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import Data.Data (Data, Typeable)
--import Data.Decimal
import Data.Generics.Aliases (extQ)
import Control.Monad.State (MonadState)
import qualified Control.Monad.State as State

import Types

data FamiliesCtx = FamiliesCtx { families :: [String] }
  deriving (Show, Data, Typeable)

--data DevicesCtx = DevicesCtx { devices :: String }
--  deriving (Show, Data, Typeable)

data ShortDevicesCtx = ShortDevicesCtx { shortDevices :: [String] }
  deriving (Show, Data, Typeable)

data RegsCtx = RegsCtx {
    imports :: [Text]
  , regs    :: Text
  } deriving (Show, Data, Typeable)

data PeriphCtx = PeriphCtx {
    typ           :: String
  , version       :: String
  , bitDataRegs   :: String
  , bitDataRegsMk :: String
  } deriving (Show, Data, Typeable)


hastacheConf   :: MonadIO m => MuConfig m
hastacheConf = defaultConfig {
    muEscapeFunc = emptyEscape
  }

templateRename "typ" = "type"
templateRename x = x

templateD context = template (mkGenericContext' templateRename defaultExt context)

--template :: (Data (m (MuType m)), Monad m)
--         => (T.Text -> m (MuType m))
--         -> T.Text
--         -> T.Text -> MonadGen ()
template context namespace tmpl = do
  t <- getTemplate tmpl
  out <- liftIO
      $ flip State.evalStateT True
      $ statefull t -- hastacheStr hastacheConf t composed 

  liftIO $ writeHS ns $ TL.toStrict out
  where
    ns = "Ivory.BSP." <> namespace

    statefull :: Text -> State.StateT Bool IO TL.Text
    statefull t = hastacheStr hastacheConf t composed

    composed = composeCtx (globalContext ns) context
    --composed = context


globalContext ns = composeCtx
                    (mkStrContext $ namespaceContext ns)
                    (mkStrContext prefixCtx)

namespaceContext ns "modns" = MuVariable ns
namespaceContext ns _ = MuNothing

prefixCtx "prefixRest" = MuLambdaM $ prefixRest . decodeStr
prefixCtx _ = MuNothing

prefixRest :: MonadState Bool m => String -> m String
prefixRest a = do
    first <- State.get
    if first then State.put False >> return ("    " ++ a)
             else return ("  , " ++ a)

template' :: T.Text
          -> T.Text
          -> MonadGen ()
template' = template mempty -- (mempty :: MuContext IO)

getTemplatesPath :: MonadGen Text
getTemplatesPath = do
  mPath <- liftIO $ need "TEMPLATES_PATH"
  case mPath of
    Nothing -> liftIO $ die "need TEMPLATES_PATH env var"
    Just p -> return p

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

emptyCtx :: Monad m => MuContext m
emptyCtx = mempty

--listCtx :: (Monad m) => [(String, Text)] -> MuContext m
--listCtx :: [(String, Text)] -> MuContext IO -- (MonadGen ())
listCtx l = mkStrContext $ lookup l
  where lookup [] what = MuNothing
        lookup ((k,v):xs) what | k == what = MuVariable v
        lookup ((k,v):xs) what | otherwise = lookup xs what
