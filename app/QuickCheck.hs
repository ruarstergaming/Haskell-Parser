{-# LANGUAGE TemplateHaskell #-}
module QuickCheck where
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Arbitrary
import Expr
import REPL
import BinaryTree
import Data.Maybe


instance Arbitrary a => Arbitrary (BTree a) where
    arbitrary = oneof [do
        l <- arbitrary
        a <- arbitrary
        Node l a <$> arbitrary,return Leaf]


prop_addToTree :: (Ord a) => a -> BTree a -> Bool
prop_addToTree v t = isJust (getElem (insert t v) v)


prop_ADDInts :: Int -> Int -> Bool
prop_ADDInts x y  = eval (vars initLState) (Add (Val (IntVal x)) (Val (IntVal y)))  == Just (IntVal (x + y))

prop_ADDFloats :: Float -> Float -> Bool
prop_ADDFloats x y  = eval (vars initLState) (Add (Val (FloatVal x)) (Val (FloatVal y)))  == Just (FloatVal (x + y))

prop_ADDStrings:: String -> String -> Bool
prop_ADDStrings x y  = eval (vars initLState) (Add (Val (StrVal x)) (Val (StrVal y)))  == Just (StrVal (x ++ y))

prop_SubIntvals :: Int -> Int -> Bool
prop_SubIntvals x y  = eval (vars initLState) (Sub (Val (IntVal x)) (Val (IntVal y)))  == Just (IntVal (x - y))

prop_SubFloats :: Float -> Float -> Bool
prop_SubFloats x y  = eval (vars initLState) (Sub (Val (FloatVal x)) (Val (FloatVal y)))  == Just (FloatVal (x - y))

prop_MultInt :: Int -> Int -> Bool
prop_MultInt x y = eval (vars initLState) (Mult (Val (IntVal x)) (Val (IntVal y)))  == Just (IntVal (x * y))

prop_MultFloat :: Float -> Float -> Bool
prop_MultFloat x y = eval (vars initLState) (Mult (Val (FloatVal x)) (Val (FloatVal y)))  == Just (FloatVal (x * y))




return []
runTests = $quickCheckAll

