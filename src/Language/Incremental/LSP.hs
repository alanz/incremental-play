{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings, TypeOperators, InstanceSigs #-}
-- NoImplicitPrelude
module Language.Incremental.LSP
 ( main
 ) where

-- import Data.Maybe
-- import Data.Semigroup
-- import Language.Haskell.LSP.Diagnostics
-- import Protolude hiding (sourceLine, sourceColumn)
-- import Text.Megaparsec (errorPos, parse, SourcePos(..), unPos, parseErrorTextPretty)
-- import Text.PrettyPrint.ANSI.Leijen (renderPretty, displayS)
-- import qualified Data.List.NonEmpty as NonEmpty
-- import qualified Data.Text as T
import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import qualified Control.Exception as Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.STM
import           Data.Default
import qualified Language.Haskell.LSP.Control as LSP.Control
import qualified Language.Haskell.LSP.Core as LSP.Core
import           Language.Haskell.LSP.Messages
import qualified Language.Haskell.LSP.Types as LSP
import           Language.Haskell.LSP.Types.Capabilities as C
import qualified Language.Haskell.LSP.Types.Lens as LSP
import qualified Language.Haskell.LSP.Utility as LSP
import           Language.Incremental.Visualise
import           Options.Applicative
import           Repetitive3
import           System.Exit
import qualified System.Log.Logger as L
import           System.Posix.Process
import           System.Posix.Types
import qualified Language.Haskell.LSP.VFS as LSP
import qualified Yi.Rope as Yi

-- ---------------------------------------------------------------------



data CommandLineOptions
   = CommandLineOptions
   { serverLogFile :: FilePath
   , sessionLogFile :: FilePath
   }

commandLineOptionsParser :: Parser CommandLineOptions
commandLineOptionsParser = CommandLineOptions
   <$> strOption
       ( long "server-log-file"
       <> metavar "FILENAME"
       <> help "Log file used for general server logging"
       <> value "/tmp/inc-lsp.log"
       )
   <*> strOption
       ( long "session-log-file"
       <> metavar "FILENAME"
       <> help "Log file used for general server logging"
       <> value "/tmp/inc-lsp.log"
       )

commandLineOptions :: ProcessID -> ParserInfo CommandLineOptions
commandLineOptions _ = info (commandLineOptionsParser <**> helper)
   ( fullDesc
   <> header "inc-lsp"
   <> progDesc "A Language Server Protocol Implementation for experimental incremental parsers"
   )

main :: IO ()
main = do
 opts <- execParser =<< commandLineOptions <$> getProcessID
 exitcode <- run opts (return ())
 case exitcode of
   0 -> exitSuccess
   c -> exitWith . ExitFailure $ c

run :: CommandLineOptions -> IO () -> IO Int
run opts dispatcherProc = flip Exception.catches handlers $ do
   rin <- atomically newTChan :: IO (TChan ReactorInput)
   let dp lf = do
           _rpid <- forkIO $ reactor lf rin
           dispatcherProc
           return Nothing
   flip Exception.finally L.removeAllHandlers $ do
       -- LSP.Core.setupLogger (Just (serverLogFile opts)) [] L.DEBUG
       LSP.Core.setupLogger Nothing [] L.DEBUG
       LSP.Control.run
           (return (Right ()), dp)
           (lspHandlers rin)
           lspOptions
           (Just (sessionLogFile opts))
 where
   handlers =
     [ Exception.Handler ioExcept
     , Exception.Handler someExcept]
   ioExcept (e :: Exception.IOException) = print e >> return 1
   someExcept (e :: Exception.SomeException) = print e >> return 1

-- The reactor is a process that serialises and buffers all requests from the
-- LSP client, so they can be sent to the backend compiler one at a time, and a
-- reply sent.
newtype ReactorInput =
  HandlerRequest FromClientMessage -- ^ injected into the reactor
                                   -- input by each of the individual
                                   -- callback handlers

-- | The monad used in the reactor
type R c a = ReaderT (LSP.Core.LspFuncs c) IO a

-- | The single point that all events flow through, allowing management of state
-- to stitch replies and requests together from the two asynchronous sides: lsp
-- server and backend compiler
reactor :: LSP.Core.LspFuncs () -> TChan ReactorInput -> IO ()
reactor lf inp =
   flip runReaderT lf $ forever $ do
       inval <- liftIO $ atomically $ readTChan inp
       case inval of
           HandlerRequest (RspFromClient rm) ->
             liftIO $ LSP.logs $ "reactor:got RspFromClient:" ++ show rm

           HandlerRequest (NotDidOpenTextDocument _notification) -> do
             liftIO $ LSP.logs "****** reactor: processing NotDidOpenTextDocument"

           HandlerRequest (NotDidChangeTextDocument p) -> do
             liftIO $ LSP.logs $ "****** reactor: processing NotDidChangeTextDocument" ++ show p
             return ()

           HandlerRequest (NotInitialized _) ->
             return ()

           HandlerRequest (NotDidSaveTextDocument _notification) -> do
             liftIO $ LSP.Core.flushDiagnosticsBySourceFunc lf 200 (Just "inc")
             return ()

           -- --------------------------

           HandlerRequest (ReqDocumentSymbols req) -> do
             liftIO $ LSP.logs $ "reactor:got Document symbol request:" ++ show req
             -- C.ClientCapabilities _ tdc _ <- asksLspFuncs LSP.Core.clientCapabilities
             let
               uri = req ^. LSP.params . LSP.textDocument . LSP.uri

               vfsFunc = LSP.Core.getVirtualFileFunc lf

             mf <- liftIO $ vfsFunc uri
             txt <- case mf of
               Nothing -> error $ "ReqDocumentSymbols: no valid file for:" ++ show uri
               Just (LSP.VirtualFile v yitext) -> return $ Yi.toString yitext

             let
               -- syms = asHierarchy "a BbDd c"
               syms = asHierarchy txt
             {-
  DocumentSymbol
    { _name           :: Text -- ^ The name of this symbol.
    -- | More detail for this symbol, e.g the signature of a function. If not
    -- provided the name is used.
    , _detail         :: Maybe Text
    , _kind           :: SymbolKind -- ^ The kind of this symbol.
    , _deprecated     :: Maybe Bool -- ^ Indicates if this symbol is deprecated.
    -- | The range enclosing this symbol not including leading/trailing
    -- whitespace but everything else like comments. This information is
    -- typically used to determine if the the clients cursor is inside the symbol
    -- to reveal in the symbol in the UI.
    , _range          :: Range
    -- | The range that should be selected and revealed when this symbol is being
    -- picked, e.g the name of a function. Must be contained by the the '_range'.
    , _selectionRange :: Range
    -- | Children of this symbol, e.g. properties of a class.
    , _children       :: Maybe (List DocumentSymbol)

-}
             reactorSend $ RspDocumentSymbols
                         $ LSP.Core.makeResponseMessage req (LSP.DSDocumentSymbols (LSP.List syms))

           -- --------------------------

           HandlerRequest (ReqHover req) -> do
             liftIO $ LSP.logs $ "reactor:got HoverRequest:" ++ show req
             let params = req ^. LSP.params
                 pos = params ^. LSP.position
                 uri = params ^. LSP.textDocument . LSP.uri
                 vfsFunc = LSP.Core.getVirtualFileFunc lf

             mf <- liftIO $ vfsFunc uri
             txt <- case mf of
               Nothing -> error $ "ReqHover: no valid file for:" ++ show uri
               Just (LSP.VirtualFile v yitext) -> return $ Yi.toString yitext

             let mh = Nothing
             reactorSend $ RspHover
                         $ LSP.Core.makeResponseMessage req mh

           -- --------------------------

           HandlerRequest req ->
             liftIO $ LSP.logs $ "reactor: unhandled HandlerRequest:" ++ show req

reqToURI :: (LSP.HasParams s a1, LSP.HasTextDocument a1 a2,
             LSP.HasUri a2 a3)
             => s -> a3
reqToURI req =
    req ^.
    LSP.params .
    LSP.textDocument .
    LSP.uri

reactorSend :: FromServerMessage -> R () ()
reactorSend msg = do
  lf <- ask
  liftIO $ LSP.Core.sendFunc lf msg

lspOptions :: LSP.Core.Options
lspOptions = def
  { LSP.Core.textDocumentSync = Just syncOptions }
  where
    syncOptions :: LSP.TextDocumentSyncOptions
    syncOptions = LSP.TextDocumentSyncOptions
       { LSP._openClose         = Just False
       , LSP._change            = Just LSP.TdSyncIncremental
       , LSP._willSave          = Just False
       , LSP._willSaveWaitUntil = Just False
       , LSP._save              = Just $ LSP.SaveOptions $ Just False
       }

lspHandlers :: TChan ReactorInput -> LSP.Core.Handlers
lspHandlers rin = def
   { LSP.Core.initializedHandler                       = Just $ passHandler NotInitialized
   , LSP.Core.didChangeTextDocumentNotificationHandler = Just $ passHandler NotDidChangeTextDocument
   , LSP.Core.didSaveTextDocumentNotificationHandler   = Just $ passHandler NotDidSaveTextDocument
   , LSP.Core.documentFormattingHandler                = Just $ passHandler ReqDocumentFormatting
   , LSP.Core.didOpenTextDocumentNotificationHandler   = Just $ passHandler NotDidOpenTextDocument
   , LSP.Core.documentSymbolHandler                    = Just $ passHandler ReqDocumentSymbols
   , LSP.Core.hoverHandler                             = Just $ passHandler ReqHover


   }
   where
     passHandler :: (a -> FromClientMessage) -> LSP.Core.Handler a
     passHandler c notification =
       atomically $ writeTChan rin (HandlerRequest (c notification))
