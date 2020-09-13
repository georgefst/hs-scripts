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
import Data.Foldable (Foldable (toList))
import Data.List (stripPrefix)
import EnumSet (EnumSet)
import EnumSet qualified
import Lexer (P (unP), PState (PState), ParseResult (..), ParserFlags (ParserFlags), mkPState)
import Options.Generic (Generic, ParseRecord, getRecord)
import Parser qualified
import StringBuffer (stringToStringBuffer)
import System.IO.Unsafe (unsafePerformIO)
import TcEvidence (HsWrapper (..))
import Text.Pretty.Simple.Internal (Expr (StringLit), Expr (ErrorExpr), defaultPostProcess, makePostProcessor)
import Type.Reflection (Typeable, typeRep)

import GHC
import GhcPlugins hiding ((<>), Expr)
import Text.Pretty.Simple

data Args = Args
    { inFile :: String
    }
    deriving (Eq, Ord, Show, Generic, ParseRecord)

main :: IO ()
main = do
    Args {..} <- getRecord "ghc-parse"
    contents <- readFile inFile
    let parseRes = unP Parser.parseModule parseState
        location = mkRealSrcLoc (mkFastString inFile) 1 1
        parseState = mkPState unsafeGlobalDynFlags (stringToStringBuffer contents) location
    pPrintOpt NoCheckColorTty printOpts parseRes

printOpts :: OutputOptions
printOpts =
    defaultOutputOptionsDarkBg
        { outputOptionsCompact = True,
          outputOptionsPageWidth = 200,
          outputOptionsPostProcess = defaultPostProcess EscapeNonPrintable . myPostProcess
        }

--TODO hmm, not sure I like 'makePostProcessor'
myPostProcess :: [Expr] -> [Expr]
myPostProcess = makePostProcessor \case
    StringLit (stripPrefix "ERROR" -> Just s) -> Just $ ErrorExpr s
    _ -> Nothing

showOutputable :: Outputable a => a -> String
showOutputable = show . flip (renderWithStyle unsafeGlobalDynFlags) (mkCodeStyle CStyle) . ppr

{-# NOINLINE typeRep' #-}
typeRep' :: forall a. Typeable a => String
typeRep' = unsafePerformIO $ appendFile "tmp" ("\nderiving instance Show (" <> t <> ")") >> pure t
    where
        t = show $ typeRep @a

instance {-# OVERLAPPABLE #-} Typeable a => Show a where
    show = const $ show $ "ERROR" <> typeRep' @a

instance Show a => Show (DynFlags -> a) where
    show = show . ($ unsafeGlobalDynFlags)
instance Show a => Show (Bag a) where
    show b = "Bag.listToBag " ++ show (toList b)
instance (Enum a, Show a) => Show (EnumSet a) where
    show b = "EnumSet.fromList" ++ show (EnumSet.toList b)
deriving instance Show a => Show (Located a)

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
