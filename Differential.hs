{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-
Module: Differential

Includes functions for handling differential equations

Author:  Anshul Kharbanda
Created: 5 - 21 - 2017
-}
module Differential
    ( linearDiff
    , scalarDiff
    , order2Diff
    , solve
    , solveIncludeTime
    , euler
    , rungeKutta2
    , rungeKutta4
    ) where

-- Imports
import DelimeterTable
import SimpleLinear

-- ========================================================= Create Diff Eq's =========================================================

-- |Matrix transformation differential equation
linearDiff :: (LTransform m v, Vector v, Num a) => -- ^Uses Num types an Vector and LTransform type constructors
           m a -- ^The transformation matrix
           -> v a -- ^The step vector
           -> (a -> a) -- ^The step function
           -> v a -- ^The current x
           -> a -- ^The current t
           -> v a -- ^Returns the differential of x
linearDiff m b f x t = (m `transform` x) `vadd` ((f t) `vscale` b)

-- |Scalar value linear differential equation
scalarDiff :: (Num a) => -- ^Uses Num types
           a -- ^The scalar constant for x
           -> a -- ^The scalar constant for t
           -> Scalar a -- ^The current x
           -> a -- ^The current t
           -> Scalar a -- ^Returns the differential of x
scalarDiff r s x t = (r `vscale` x) `vadd` (s `vscale` (S t))

-- |Second order linear differential equation
order2Diff :: (Fractional a) => -- ^Uses Fractional types
           a -- ^The first coefficient
           -> a -- ^The second coefficient
           -> a -- ^The third coefficient
           -> (a -> a) -- ^The step function
           -> Vector2 a -- ^The current x
           -> a -- ^The current t
           -> Vector2 a -- ^Returns the differential of x
order2Diff a b c f x t = linearDiff (M2x2 0 1 (-c/a) (-b/a)) (V2 0 1) f x t

-- ========================================================= Numerical Solver =========================================================

-- |Numerically solves a linear differential function over a given t range using the given linear numerical solver
solve :: (Vector v, Num a, Ord a) => -- ^Uses RealFrac types and Vector type constructors
       ((v a -> a -> v a) -> v a -> a -> a -> v a) -- ^The numerical solver
       -> (v a -> a -> v a) -- ^The linear differential equation being solved
       -> v a -- ^The initial x
       -> a -- ^The initial t
       -> a -- ^The final t
       -> a -- ^The change dt
       -> [v a] -- ^Returns the numerical solution array
solve solver v' v0 t0 tf dt | (t0 >= tf) = []
                            | otherwise = v0 : (solve solver v' v1 t1 tf dt)
                                where
                                    v1 = solver v' v0 t0 dt
                                    t1 = t0 + dt

-- |Like solve, but returns an array of tuples which include time and input value at each state
solveIncludeTime :: (Vector v, Num a, Ord a) => -- ^Uses RealFrac types and Vector type constructors
       ((v a -> a -> v a) -> v a -> a -> a -> v a) -- ^The numerical solver
       -> (v a -> a -> v a) -- ^The linear differential equation being solved
       -> v a -- ^The initial x
       -> a -- ^The initial t
       -> a -- ^The final t
       -> a -- ^The change dt
       -> [(a, v a)] -- ^Returns the numerical solution array (which includes time)
solveIncludeTime solver v' v0 t0 tf dt | (t0 >= tf) = []
                                       | otherwise = (t0, v0) : (solveIncludeTime solver v' v1 t1 tf dt)
                                            where
                                                v1 = solver v' v0 t0 dt
                                                t1 = t0 + dt

-- |RowDataStruct instance for time-vector pair returned by solveIncludeTime
instance (Show a, RowDataStruct (v a)) => RowDataStruct (a, v a) where
    -- |Returns the cells of the row for the given time-vector pair
    rowCells (t, x) = (show t) : rowCells x

-- ========================================== Numerical Solvers ==========================================

-- |Euler's method of linear numerical differential solving
euler :: (Vector v, Num a) => -- ^Uses RealFrac types and Vector type constructors
        (v a -> a -> v a) -- ^The linear differential equation being solved
        -> v a  -- ^The initial x
        -> a -- ^The initial t
        -> a -- ^The final t
        -> v a -- ^Returns the step change in x
euler v' v t dt = v `vadd` (dt `vscale` (v' v t))

-- |Second Order Runge-Kutta method of linear numerical differential solving
rungeKutta2 :: (Vector v, Fractional a) => -- ^Uses RealFrac types and Vector type constructors
            (v a -> a -> v a) -- ^The linear differential equation being solved
            -> v a  -- ^The initial x
            -> a -- ^The initial t
            -> a -- ^The final t
            -> v a -- ^Returns the step change in x
rungeKutta2 v' v t dt = v `vadd` ((dt/2) `vscale` (k1 `vadd` k2))
                        where
                            k1 = v' v t
                            k2 = v' (v `vadd` (dt `vscale` k1)) (t + dt)

-- |Fourth Order Runge-Kutta method of linear numerical differential solving
rungeKutta4 :: (Vector v, Fractional a) => -- ^Uses RealFrac types and Vector type constructors
            (v a -> a -> v a) -- ^The linear differential equation being solved
            -> v a  -- ^The initial x
            -> a -- ^The initial t
            -> a -- ^The change in t
            -> v a -- ^Returns the step change in x
rungeKutta4 v' v t dt = v `vadd` ((dt/6) `vscale` (k1 `vadd` (2 `vscale` k2) `vadd` (2 `vscale` k3) `vadd` k4))
                        where
                            k1 = v' v t
                            k2 = v' (v `vadd` ((dt/2) `vscale` k1)) (t + dt/2)
                            k3 = v' (v `vadd` ((dt/2) `vscale` k2)) (t + dt/2)
                            k4 = v' (v `vadd` (dt `vscale` k3)) (t + dt)
