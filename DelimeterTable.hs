{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-
Module: DelimeterTable

Includes the delimeter table type and RowDataStruct typeclass
as well as helper functions for constructing delimeter tables.

Author:  Anshul Kharbanda
Created: 7 - 30 - 2017
-}
module DelimeterTable where

-- Imports
import Data.List

-- |Represents a structure which can be represented as a row in a DelimeterTable
class RowDataStruct a where
    -- |Converts the struct into a row of data cells
    rowCells :: a -- ^The data struct
             -> [String] -- ^The string cell array

-- |Represents a DelimeterTable of data of the given type
data DelimeterTable a where
    -- |Creates a DelimeterTable
    Table :: (RowDataStruct a) => -- ^Requires RowDataStruct types
             { delim :: String -- ^The delimeter of the table
             , header :: [String] -- ^The header string of the table
             , rows :: [a] -- ^The rows of the table
             } -> DelimeterTable a -- ^Returns a DelimeterTable

-- |Instance of Show for DelimeterTable
instance (RowDataStruct a) => Show (DelimeterTable a) where
    -- |Show DelimeterTable
    show (Table {delim=d, header=h, rows=rdata}) = lineJoin $ fmap delimit $ h:rstrings
                                                   where
                                                       lineJoin = intercalate "\n"
                                                       delimit = intercalate d
                                                       rstrings = fmap rowCells rdata

-- |Creates a csv table
csv :: (RowDataStruct a) => -- ^Requires RowDataStruct types
       [String] -- ^The array of header strings
       -> [a] -- ^The array of data values
       -> DelimeterTable a -- ^Returns the DelimeterTable
csv header rowData = Table { delim=",", header=header, rows=rowData }
