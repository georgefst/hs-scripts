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
import Data.Char (isSpace)
import Data.Foldable (forM_, toList, traverse_)
import Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef)
import Data.List (dropWhileEnd, stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Lazy.IO qualified as TL
import EnumSet (EnumSet)
import EnumSet qualified
import GHC.Hs.Dump (BlankSrcSpan (BlankSrcSpan), showAstData)
import Lexer (P (unP), PState (PState), ParseResult (..), ParserFlags (ParserFlags), mkPState)
import Options.Generic (Generic, ParseField, ParseFields, ParseRecord, getRecord)
import Parser qualified
import Prettyprinter.Lucid (renderHtml)
import Prettyprinter.Render.Util.SimpleDocTree (treeForm)
import StringBuffer (stringToStringBuffer)
import System.Console.ANSI (getTerminalSize)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)
import TcEvidence (HsWrapper (..))
import Text.Pretty.Simple.Internal (Expr (CustomExpr, StringLit), defaultPostProcess, layoutString, makePostProcessor)
import Type.Reflection (Typeable, typeRep)

import GHC
import GhcPlugins hiding (Expr, (<>))
import Lucid
import Text.Pretty.Simple

import Util.Util

data Args = Args
    { inFile :: String
    , printDerives :: Maybe FilePath
    , outHtml :: Maybe FilePath
    , hideLocs :: Bool
    , haddock :: Bool
    , mode :: Mode
    }
    deriving (Eq, Ord, Show, Generic, ParseRecord)

data Mode
    = Normal
    | DataDump
    | LocsOnly
    deriving (Eq, Ord, Show, Read, Generic, ParseRecord, ParseField, ParseFields)

data GlobalState = GlobalState
    { dynFlags :: DynFlags
    , args :: Args
    }

main :: IO ()
main = do
    args@(Args{..}) <- getRecord "ghc-parse"
    contents <- readFile inFile
    ghcLibDir <- readProcess "ghc" ["--print-libdir"] ""
    --TODO this might not always be exactly what we want - see 'initGhcMonad' haddock
    pr <- runGhc (Just $ dropWhileEnd isSpace ghcLibDir) do
        dynFlags <- haddock $? setGeneralFlag' GHC.Opt_Haddock <$> getDynFlags
        liftIO $ atomicWriteIORef globalStateRef GlobalState{..}
        pure
            . unP Parser.parseModule
            . mkPState dynFlags (stringToStringBuffer contents)
            $ mkRealSrcLoc (mkFastString inFile) 1 1
    printOpts <- mkPrintOpts . fmap snd <$> getTerminalSize
    forM_ outHtml \f ->
        TL.writeFile f . renderText . body_ [style_ "background-color: #2c2f33; color: white"] $ pPrintHtml printOpts pr
    case pr of
        PFailed _ -> putStrLn "Error: parsing failed"
        POk _ p ->
            case mode of
                DataDump -> pPrintStringOpt NoCheckColorTty printOpts . renderDoc $ showAstData BlankSrcSpan p
                Normal -> pPrintOpt NoCheckColorTty printOpts p
                LocsOnly -> pPrintOpt NoCheckColorTty printOpts . map getLoc . hsmodDecls $ unLoc p

mkPrintOpts :: Maybe Int -> OutputOptions
mkPrintOpts width =
    def
        { outputOptionsCompact = True
        , outputOptionsPageWidth = fromMaybe (outputOptionsPageWidth def) width
        , outputOptionsPostProcess = defaultPostProcess EscapeNonPrintable . myPostProcess
        , outputOptionsColorOptions =
              Just
                  defaultColorOptionsDarkBg
                      { -- all the other colours are used for something
                        colorRainbowParens = [colour Magenta, colour Yellow]
                      }
        }
  where
    def = defaultOutputOptionsNoColor

myPostProcess :: [Expr] -> [Expr]
myPostProcess = makePostProcessor \case
    StringLit (stripPrefix "ERROR" -> Just s) -> CustomExpr (colour Red) s
    StringLit (stripPrefix "OUTPUTTABLE" -> Just s) -> CustomExpr (colour Cyan) s
    x -> x

colour :: Color -> Style
colour c = colorNull{styleColor = Just (c, Vivid), styleBold = True}

pPrintHtml :: Show a => OutputOptions -> a -> Html ()
pPrintHtml opts = renderHtml . fmap renderStyle . treeForm . layoutString opts . show
  where
    renderStyle (Style mc b _i _u) =
        (if b then b_ else id) . case mc of
            Nothing -> id
            Just (c, _i) -> span_ [style_ $ renderColor c]
    renderColor c = "color:" <> T.pack (show c)

renderDoc :: SDoc -> String
renderDoc d = renderWithStyle (dynFlags globalState) d (mkCodeStyle CStyle)

showOutputable :: Outputable a => a -> String
showOutputable = show . ("OUTPUTTABLE" ++) . renderDoc . ppr

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
        unwords $
            (++ ["(", show x, ")"]) $
                if hideLocs $ args globalState then [] else ["L", "(", show s, ")", ""]

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
