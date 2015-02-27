{-# LANGUAGE
    DataKinds
  , KindSignatures
  , GADTs
  , TypeOperators
  , TypeFamilies
  , TemplateHaskell
  , MultiParamTypeClasses
  #-}

module ErikHesselinkExample where

import Data.Singletons.TH

singletons [d|
  data PNat = Z | S PNat
  |]

data HList (ts :: [*]) where
  Nil :: HList '[]
  (:&) :: t -> HList ts -> HList (t ': ts)

(&) :: t -> HList ts -> HList (t ': ts)
(&) = (:&)

(&:) :: t -> u -> HList [t, u]
(&:) x y = x & y & Nil

infixr 5 :&
infixr 5 &
infixr 6 &:

type family Sel (n :: PNat) (ts :: [*]) :: Maybe * where
  Sel n     '[]       = Nothing
  Sel Z     (t ': ts) = Just t
  Sel (S n) (t ': ts) = Sel n ts

ixH :: (Sel n ts ~ Just t) => Sing n -> HList ts -> t
ixH SZ     (x :& _ ) = x
ixH (SS n) (_ :& xs) = ixH n xs
ixH _ _  = error "Impossible"

l :: HList '[Bool, Int, String]
l = True & 1 &: "hello"

f :: String
f = case l of
  (_ :& _ :& s :& Nil) -> s
  _ -> error "Impossible"

