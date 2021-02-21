{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Arrow (returnA)
import Control.Monad
import qualified Data.ByteString.Char8 as Char8
import qualified Data.List as L
import qualified Data.Map as M
import Data.Text hiding (map)
import Database
import qualified Database.PostgreSQL.Simple as PGS
import Generate
import Opaleye
import Options.Generic
import System.IO

data Args w = Args
  { dbString :: w ::: String <?> "Database to generate the types from",
    outputPath :: w ::: String <?> "The output path (including filename) for the file you want to generate. Relative paths work."
  }
  deriving (Generic)

modifiers :: Modifiers
modifiers =
  defaultModifiers
    { shortNameModifier = firstLetter
    }

instance ParseRecord (Args Wrapped) where
  parseRecord = parseRecordWithModifiers modifiers

deriving instance Show (Args Unwrapped)

allPublicColumns :: Query InformationSchemaColumnTable
allPublicColumns = proc () -> do
  columnRow <- selectTable informationSchemaColumnTable -< ()
  restrict -< table_schema columnRow .== toNullable (pgString "public")
  returnA -< columnRow

groupColumns :: [InformationSchemaColumn] -> [TableDefinition]
groupColumns cs = map (\t -> t {columns = sortColumns (columns t)}) $ M.elems tableMap
  where
    tableMap = L.foldl' collect M.empty cs
    collect m c =
      case M.lookup (table_name c) m of
        Just t -> M.insert (table_name c) (t {columns = c : columns t}) m
        Nothing -> M.insert (table_name c) (TableDefinition (table_name c) [c]) m

sortColumns :: [InformationSchemaColumn] -> [InformationSchemaColumn]
sortColumns = L.sortOn ordinal_position

generateFile :: Args Unwrapped -> IO ()
generateFile args = do
  conn <- PGS.connectPostgreSQL (Char8.pack $ dbString args)
  allColumns <- runSelect conn allPublicColumns :: IO [InformationSchemaColumn]

  h <- openFile (outputPath args) WriteMode
  hPutStrLn h . unpack $ fileHeaderText
  forM_ (groupColumns allColumns) $ \td ->
    hPutStrLn h . unpack $ fullTableText td
  hClose h

main :: IO ()
main = unwrapRecord "opaleye-gen - Generate Opaleye boilerplate" >>= generateFile
