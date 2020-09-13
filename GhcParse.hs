{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}
{-# OPTIONS_GHC -threaded #-}

module GhcParse (main) where

import Bag (Bag)
import Data.Foldable (Foldable (toList), traverse_)
import Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import EnumSet (EnumSet)
import EnumSet qualified
import Lexer (P (unP), PState (PState), ParseResult (..), ParserFlags (ParserFlags), mkPState)
import Options.Generic (Generic, ParseRecord, getRecord)
import Parser qualified
import StringBuffer (stringToStringBuffer)
import System.Console.ANSI (getTerminalSize)
import System.IO.Unsafe (unsafePerformIO)
import TcEvidence (HsWrapper (..))
import Text.Pretty.Simple.Internal (Expr (StringLit), Expr (ErrorExpr), defaultPostProcess, makePostProcessor)
import Type.Reflection (Typeable, typeRep)

import GHC
import GhcPlugins hiding ((<>), Expr)
import Text.Pretty.Simple

data Args = Args
    { inFile :: String
    , printDerives :: Maybe FilePath
    , showLocs :: Bool
    }
    deriving (Eq, Ord, Show, Generic, ParseRecord)

data GlobalState = GlobalState
    { dynFlags :: DynFlags
    , args :: Args
    }

main :: IO ()
main = do
    args@(Args {..}) <- getRecord "ghc-parse"
    contents <- readFile inFile
    --TODO don't hardcode path - see 'initGhcMonad' haddock
    pr <- runGhc (Just "/home/gthomas/.ghcup/ghc/8.10.2/lib/ghc-8.10.2") do
        dynFlags <- getDynFlags
        liftIO $ atomicWriteIORef globalStateRef GlobalState {..}
        pure
            . unP Parser.parseModule
            . mkPState dynFlags (stringToStringBuffer contents)
            $ mkRealSrcLoc (mkFastString inFile) 1 1
    termSize <- getTerminalSize
    pPrintOpt NoCheckColorTty (printOpts $ snd <$> termSize) pr

printOpts :: Maybe Int -> OutputOptions
printOpts width =
    def
        { outputOptionsCompact = True,
          outputOptionsPageWidth = fromMaybe (outputOptionsPageWidth def) width,
          outputOptionsPostProcess = defaultPostProcess EscapeNonPrintable . myPostProcess
        }
    where
        def = defaultOutputOptionsDarkBg

--TODO hmm, not sure I like 'makePostProcessor'
myPostProcess :: [Expr] -> [Expr]
myPostProcess = makePostProcessor \case
    StringLit (stripPrefix "ERROR" -> Just s) -> Just $ ErrorExpr s
    _ -> Nothing

showOutputable :: Outputable a => a -> String
showOutputable = show . flip (renderWithStyle $ dynFlags globalState) (mkCodeStyle CStyle) . ppr

-- | We need this for very similar reasons to why 'GHC.unsafeGlobalDynFlags' exists
{-# NOINLINE globalState #-}
globalState :: GlobalState
globalState = unsafePerformIO $ readIORef globalStateRef
{-# NOINLINE globalStateRef #-}
globalStateRef :: IORef GlobalState
globalStateRef = unsafePerformIO $ newIORef $ error "globalStateRef uninitialised"

{-# NOINLINE showTypeable #-}
showTypeable :: forall a. Typeable a => String
showTypeable = unsafePerformIO do
    traverse_ @Maybe (flip appendFile ("\nderiving instance Show (" <> t <> ")")) $ printDerives $ args globalState
    pure t
    where
        t = show $ typeRep @a

instance {-# OVERLAPPABLE #-} Typeable a => Show a where
    show = const $ show $ "ERROR" <> showTypeable @a

instance Show a => Show (DynFlags -> a) where
    show = show . ($ dynFlags globalState)
instance Show a => Show (Bag a) where
    show b = "Bag.listToBag " ++ show (toList b)
instance (Enum a, Show a) => Show (EnumSet a) where
    show b = "EnumSet.fromList" ++ show (EnumSet.toList b)
instance Show a => Show (Located a) where
    show (L s x) =
        unwords $ (++ ["(", show x, ")"]) $
            if showLocs $ args globalState then ["L", "(", show s, ")", ""] else []

instance Show OccName where show = showOutputable
instance Show ModuleName where show = showOutputable

deriving instance Show (DocDecl)
deriving instance Show (GhcPs)
deriving instance Show (GRHS (GhcPass 'Parsed) (GenLocated SrcSpan (HsExpr (GhcPass 'Parsed))))
deriving instance Show (GRHSs (GhcPass 'Parsed) (GenLocated SrcSpan (HsExpr (GhcPass 'Parsed))))
deriving instance Show (HsBind GhcPs)
deriving instance Show (HsDecl GhcPs)
deriving instance Show (HsExpr GhcPs)
deriving instance Show (HsLit (GhcPass 'Parsed))
deriving instance Show (HsLocalBindsLR (GhcPass 'Parsed) (GhcPass 'Parsed))
deriving instance Show (HsMatchContext RdrName)
deriving instance Show (HsModule GhcPs)
deriving instance Show (HsOverLit (GhcPass 'Parsed))
deriving instance Show (HsWrapper)
deriving instance Show (IE GhcPs)
deriving instance Show (IEWildcard)
deriving instance Show (IEWrappedName RdrName)
deriving instance Show (LexicalFixity)
deriving instance Show (Match (GhcPass 'Parsed) (GenLocated SrcSpan (HsExpr (GhcPass 'Parsed))))
deriving instance Show (MatchGroup (GhcPass 'Parsed) (GenLocated SrcSpan (HsExpr (GhcPass 'Parsed))))
deriving instance Show (NoExtCon)
deriving instance Show (NoExtField)
deriving instance Show (Origin)
deriving instance Show (OverLitVal)
deriving instance Show (ParseResult (Located (HsModule GhcPs)))
deriving instance Show (ParserFlags)
deriving instance Show (PState)
deriving instance Show (RdrName)
deriving instance Show (SrcStrictness)
deriving instance Show (StringLiteral)
deriving instance Show (WarningTxt)
