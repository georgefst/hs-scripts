{- HLINT ignore "Unused LANGUAGE pragma" -}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}
{-# OPTIONS_GHC -threaded #-}

{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use section" #-}

module AoC19 (main) where

import Control.Applicative hiding (many, some) -- TODO bit weird - why hide?
import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import Data.Foldable qualified as F
import Data.List
import Data.List.Extra
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as TL
import Data.Tuple.Extra
import Data.Void
import Debug.Pretty.Simple
import GHC.TypeLits (Symbol)
import Text.Megaparsec hiding (sepBy1)
import Text.Megaparsec.Char
import Text.Pretty.Simple

main :: IO ()
main = do
    _example <- either (error . TL.unpack . pShow) id . runParser parser "" <$> T.readFile "aoc19-example"
    input <- either (error . TL.unpack . pShow) id . runParser parser "" <$> T.readFile "aoc19-input"
    putStrLn ""
    pp
        . sum
        . map (\(Part p) -> sum $ map p enumerate)
        $ filter ((== Accept) . flip applyWorkflows input.rules) input.parts

applyWorkflows :: Part -> WorkflowMap -> Result
applyWorkflows part workflows = go "in"
  where
    go s = case applyWorkflow part $ lookupWorkflow s workflows of
        Final result -> result
        GoTo workflow -> go workflow
lookupWorkflow :: WorkflowName -> WorkflowMap -> Workflow
lookupWorkflow s = fromMaybe (error "failed to find workflow") . Map.lookup s
applyWorkflow :: Part -> Workflow -> RuleResult
applyWorkflow part Workflow{..} = fromMaybe fallback $ firstJust (uncurry $ applyRule part) rules
applyRule :: Part -> Condition -> RuleResult -> Maybe RuleResult
applyRule (Part part) Condition{..} result = if part field `op` bound then Just result else Nothing
  where
    op = case operator of
        LessThan -> (<)
        GreaterThan -> (>)

data Input = Input
    { rules :: WorkflowMap
    , parts :: [Part]
    }
    deriving (Show)
type WorkflowMap = Map WorkflowName Workflow
newtype WorkflowName = WorkflowName Text deriving newtype (Eq, Ord, Show, IsString)
data Workflow = Workflow
    { rules :: [(Condition, RuleResult)]
    , fallback :: RuleResult
    }
    deriving (Show)
data Result = Accept | Reject deriving (Eq, Show)
data RuleResult = Final Result | GoTo WorkflowName deriving (Show)
data Condition = Condition
    { field :: Field
    , operator :: Operator
    , bound :: Int
    }
    deriving (Show)
data Field = X | M | A | S deriving (Show, Bounded, Enum)
data Operator = LessThan | GreaterThan deriving (Show)
newtype Part = Part (Field -> Int)
instance Show Part where show (Part f) = show $ map (id &&& f) enumerate

type Parser = Parsec Void Text
parser :: Parser Input
parser = do
    rules <-
        Map.fromList <$> flip sepBy1 line do
            name <- nameParser
            void $ char '{'
            rules <- flip sepBy1 (char ',') do
                field <- fieldParser
                operator <- LessThan <$ char '<' <|> GreaterThan <$ char '>'
                bound <- int
                void $ char ':'
                result <- resultParser
                pure (Condition{..}, result)
            void $ char ','
            fallback <- resultParser
            void $ char '}'
            pure (name, Workflow{..})
    line
    line
    parts <- flip sepBy1 line do
        void $ char '{'
        x <- mappingParser 'x'
        void $ char ','
        m <- mappingParser 'm'
        void $ char ','
        a <- mappingParser 'a'
        void $ char ','
        s <- mappingParser 's'
        void $ char '}'
        pure $ Part \case
            X -> x
            M -> m
            A -> a
            S -> s
    pure Input{..}
  where
    mappingParser c = char c *> char '=' *> int
    nameParser = WorkflowName . T.pack <$> some (satisfy isAlpha)
    resultParser =
        asum
            [ Final Accept <$ char 'A'
            , Final Reject <$ char 'R'
            , GoTo <$> nameParser
            ]
    fieldParser =
        asum
            [ X <$ char 'x'
            , M <$ char 'm'
            , A <$ char 'a'
            , S <$ char 's'
            ]
    int = read <$> some digitChar <?> "int"
    line = void newline

pp :: (MonadIO m, Show a) => a -> m ()
-- pp = pPrintOpt NoCheckColorTty defaultOutputOptionsDarkBg{outputOptionsCompact = True}
pp = pPrintForceColor

sepBy1 :: (Alternative m, MonadPlus m, MonadParsec e0 s0 m) => m a -> m sep -> m [a]
sepBy1 p sep = F.toList <$> sepByNonEmpty
  where
    sepByNonEmpty = (:|) <$> p <*> many (try $ sep *> p)
