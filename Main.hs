{-# LANGUAGE FlexibleInstances #-}
{-
Module: Main

Creates a csv table of the numerical solution of an order 2 linear differential model
given via standard input, and pipes the string to the standard output

Author:  Anshul Kharbanda
Created: 7 - 29 - 2017
-}
module Main where

-- Inports
import DelimeterTable
import SimpleLinear
import Differential

-- |Returns the numerical solution for the given model
modeler :: (Vector2 Float -> Float -> Vector2 Float) -> -- ^The differential equation model
        -> Vector2 Float -- ^The initial x
        -> Float -- ^The initial t
        -> Float -- ^The final t
        -> Float -- ^The change in t with each step
        -> [(Float, Vector2 Float)] -- ^Returns numerical solution array
modeler = solveIncludeTime euler

-- |Models an order 2 linear equation with a linear step function
--
-- Model: a*y'' + b*y' + c*y = s*t
modelO2LS :: Float -- The first coefficient of the characteristic
          -> Float -- The second coefficient of the characteristic
          -> Float -- The third coefficient of the characteristic
          -> Float -- The scalar value of the linear step function
          -> Float -- The initial x value
          -> Float -- The initial v (or x') value
          -> Float -- The initial t value
          -> Float -- The final t value
          -> Float -- The change in t with each step
          -> [(Float, Vector2 Float)] -- ^Returns numerical solution array
modelO2LS a b c s x0 v0 t0 tf dt = modeler r' r0 t0 tf dt
                                 where
                                     r' = order2Diff a b c (s*)
                                     r0 = V2 x0 v0

-- |Applies the given function of 9 input values to the array of 9 args
doFromArgs :: (a -> a -> a -> a -> a -> a -> a -> a -> a -> b) -- ^The 9 argument function
           -> [a] -- ^The input arguments
           -> b -- ^Result from function
doFromArgs func args = func x0 x1 x2 x3 x4 x5 x6 x7 x8
                     where
                         x0 = args !! 0
                         x1 = args !! 1
                         x2 = args !! 2
                         x3 = args !! 3
                         x4 = args !! 4
                         x5 = args !! 5
                         x6 = args !! 6
                         x7 = args !! 7
                         x8 = args !! 8

-- |Parses a string into an array of command line arguments
parseArgs :: (Read a) => -- ^Can only parse readable types
             String -- ^The string of arguments
             -> [a] -- ^Returns the array of arguments
parseArgs astr = fmap read $ words astr

-- |Creates a csv table from the given numerical solution array
createCSVofSolution :: (RowDataStruct a) => -- ^Requires data points that can be converted into DelimeterTable rows
                    [a] -- ^The array of solution points
                    -> DelimeterTable -- ^Returns a DelimeterTable
createCSVofSolution = csv ["t", "x1", "x2"]

-- |Returns a csv table string from the given string of model arguments
createCSVofModelFromCommandArgs :: String -- ^Model input arguments
                                -> String -- ^The csv table string
createCSVofModelFromCommandArgs mdata = show $ createCSVofSolution $ doFromArgs modelO2LS $ parseArgs mdata

-- |Runs createCSVofModelFromCommandArgs through standard in -> out
main :: IO ()
main = interact createCSVofModelFromCommandArgs
