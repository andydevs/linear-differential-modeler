{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-
Module: SimpleLinear

Simple types which implement Linear algebra functionality

Author:  Anshul Kharbanda
Created: 7 - 30 - 2017
-}
module SimpleLinear where

-- Imports
import DelimeterTable

-- ================= TypeClasses ==================

-- |Represents vector types
class Vector v where
    -- |Adds two vectors
    vadd :: (Num a) => -- ^Requires numeric types
            v a -- ^The first vector to add
            -> v a -- ^The second vector to add
            -> v a -- ^The sum of the two vectors

    -- |Scales a vector
    vscale :: (Num a) => -- ^Requires numeric types
              a -- ^The scalar value
              -> v a -- ^The vector to scale
              -> v a -- ^The scaled vector

    -- |Returns the zero vector
    vzero :: (Num a) => -- ^Requires numeric types
             v a -- ^The zero vector

    -- |Returns the negative vector
    vnegate :: (Num a) => -- ^Requires numeric types
               v a -- ^The vector to negate
               -> v a -- ^The negated vector

    -- |Returns the zero scalar
    szero :: (Num a) => -- ^Requires numeric types
             a -- ^Returns the zero scalar

    -- |Returns the identity scalar
    sid :: (Num a) => -- ^Requires numeric types
           a -- ^Returns the identity scalar

-- |Represents dottable types
-- | Requires an r (the parameter type class), and an s (the result type class)
class Dottable r s where
    -- |Performs the dot operation
    dot :: (Num a) => -- ^Requires numerical type
           r a -- ^The first parameter
           -> r a -- ^The second parameter
           -> s a -- ^Returns the dotted value

-- |Represents linear transformer types
-- | Requires an m (the transformer type class), and a v (the transformed type class)
class LTransform m v where
    -- |Performs a linear transformation
    transform :: (Num a) => -- ^Requires numeric types
                 m a -- ^The transformer parameter
                 -> v a -- ^The transformed parameter
                 -> v a -- ^Returns the transformed result

    -- |Returns the identity transformation
    identity :: (Num a) => -- ^Requires numeric types
                m a -- ^Returns the identity transformer

-- ==================== Scalar ====================
-- |The Scalar data type
data Scalar a = S a

-- |Show instance for Scalar type
instance (Show a) => Show (Scalar a) where
    -- |Shows the scalar
    show (S s) = show s

-- |Vector instance for Scalar type
instance Vector Scalar where
    -- |Adds two scalars
    (S x) `vadd` (S y) = S $ x+y

    -- |Scales a scalar
    s `vscale` (S x) = S $ s*x

    -- |Returns the zero scalar
    vzero = S 0

    -- |Returns the negative scalar
    vnegate (S x) = S (negate x)

    -- |Returns the zero scalar
    szero = 0

    -- |Returns the identity scalar
    sid = 1

-- |LTransform instance for Scalar and Scalar types
instance LTransform Scalar Scalar where
    -- |Performs a scalar linear transformation
    (S s) `transform` (S t) = S (s*t)

    -- |Returns the identity scalar transformation
    identity = S 1

-- |LTransform instance for Scalar and Vector2 types
instance LTransform Scalar Vector2 where
    -- |Performs a scalar linear transformation
    (S s) `transform` v = s `vscale` v

    -- |Returns the identity scalar transformation
    identity = S 1

-- |Functor instance for Scalar types
instance Functor Scalar where
    -- |Maps the scalar with the given function
    fmap f (S x) = S (f x)

-- ==================== Vector2 ====================
-- |Vector2 datatype
data Vector2 a = V2 a a

-- |Show instance for Vector2 type
instance (Show a) => Show (Vector2 a) where
    -- |Shows the scalar
    show (V2 x1 x2) = "<" ++ (show x1) ++ "," ++ (show x2) ++ ">"

-- |Vector instance for Vector2 type
instance Vector Vector2 where
    -- |Adds two Vector2's
    (V2 x1 x2) `vadd` (V2 y1 y2) = V2 (x1+y1) (x2+y2)

    -- |Scales a Vector2
    s `vscale` (V2 x1 x2) = V2 (s*x1) (s*x2)

    -- |Returns the zero Vector2
    vzero = V2 0 0

    -- |Returns the negative Vector2
    vnegate (V2 x1 x2) = V2 (negate x1) (negate x2)

    -- |Returns the zero scalar
    szero = 0

    -- |Returns the identity scalar
    sid = 1

-- |Functor instance for Vector2 types
instance Functor Vector2 where
    -- |Maps the scalar with the given function
    fmap f (V2 x1 x2) = V2 (f x1) (f x2)

-- |RowDataStruct instance for vector types
instance (Show a) => RowDataStruct (Vector2 a) where
    -- |Converts the Vector2 into a row of data cells
    rowCells (V2 x1 x2) = fmap show [x1, x2]

-- ============================================ Matrix2x2 ============================================
-- |Matrix2x2 data type
data Matrix2x2 a = M2x2 a a a a deriving (Show)

-- |Vector instance of Matrix2x2
instance Vector Matrix2x2 where
    -- |Adds two Matrix2x2's
    (M2x2 x11 x12 x21 x22) `vadd` (M2x2 y11 y12 y21 y22) = M2x2 (x11+y11) (x12+y12) (x21+y21) (x22+y22)

    -- |Scales a Matrix2x2
    s `vscale` (M2x2 x11 x12 x21 x22) = M2x2 (s*x11) (s*x12) (s*x21) (s*x22)

    -- |Returns the zero Matrix2x2
    vzero = M2x2 0 0 0 0

    -- |Returns the negative Matrix2x2
    vnegate (M2x2 x11 x12 x21 x22) = M2x2 (negate x11) (negate x12) (negate x21) (negate x22)

    -- |Returns the zero scalar
    szero = 0

    -- |Returns the identity scalar
    sid = 1

-- |Linear Transformer instance of Matrix2x2
instance LTransform Matrix2x2 Vector2 where
    -- |Performs the matrix linear transformation
    (M2x2 x11 x12 x21 x22) `transform` (V2 x1 x2) = V2 (x1*x11 + x2*x12) (x1*x21 + x2*x22)

    -- |Returns the identity Matrix2x2
    identity = M2x2 1 0 0 1

-- |Functor instance for Matrix2x2 types
instance Functor Matrix2x2 where
    -- |Converts the Matrix2x2 into a row of data cells
    fmap f (M2x2 x11 x12 x21 x22) = M2x2 (f x11) (f x12) (f x21) (f x22)
