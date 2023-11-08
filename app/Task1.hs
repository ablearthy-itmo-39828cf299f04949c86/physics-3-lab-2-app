{-# LANGUAGE OverloadedStrings #-}
module Task1 (main) where

import           Control.Monad (forM_)
import           Fmt
import           GHC.Float     (int2Double)
import qualified Numeric.AD    as D

h₀ = 0.191
h₀' = 0.193

yF x₁ x₂ = x₂ - x₁
zF t₁ t₂ = (t₂^2 - t₁^2)/2

δY :: Double -> Double -> Double
δY x₁ x₂ = sqrt $ (pd₁ * δx₁)^2 + (pd₂ * δx₂)^2
  where
    δx₁ = 2 / 3 * 5 / 1000
    δx₂ = 2 / 3 * 5 / 1000

    pd₁ = D.diff (\x1 -> yF x1 (D.auto x₂)) x₁
    pd₂ = D.diff (\x2 -> yF (D.auto x₁) x2) x₂

δZ :: Double -> Double -> Double
δZ t₁ t₂ = sqrt $ (pd₁ * δt₁)^2 + (pd₂ * δt₂)^2
  where
    δt₁ = 2 / 3 * 0.1
    δt₂ = 2 / 3 * 0.1

    pd₁ = D.diff (\t1 -> zF t1 (D.auto t₂)) t₁
    pd₂ = D.diff (\t2 -> zF (D.auto t₁) t2) t₂

dataset =
  [ ((0.15, 0.4), (1.4, 2.6))
  , ((0.15, 0.5), (1.5, 3.1))
  , ((0.15, 0.7), (1.4, 3.7))
  , ((0.15, 0.9), (1.3, 4.0))
  , ((0.15, 1.1), (1.4, 4.6))
  ]

ys = [ (yF x₁ x₂, δY x₁ x₂) | ((x₁, x₂), _) <- dataset]
zs = [ (zF t₁ t₂, δZ t₁ t₂) | (_, (t₁, t₂)) <- dataset]

pprintNum :: Int -> Double -> Double -> String
pprintNum prec a δa = "" +| (fixedF prec a) |+ "±" +| (fixedF prec δa) |+ ""

theA = sum (zipWith (*) (map fst ys) (map fst zs)) / sum (map (^2) (map fst zs))

σa = sqrt $ s₁ / (cnt - 1) / s₂
  where
    s₁ = sum $ zipWith (\y z -> (y - theA * z)^2) (map fst ys) (map fst zs)
    s₂ = sum $ map (^2) (map fst zs)
    cnt = int2Double $ length ys

δa = 2 * σa

main :: IO ()
main = do
  putStrLn "x₁ x₂ t₁ t₂ Y Z"
  let prec = 2
  forM_ (zip3 dataset ys zs) $ \(((x₁, x₂), (t₁, t₂)), (y, δy), (z, δz)) -> do
    putStrLn $ "" +| (fixedF prec x₁) |+ " " +| (fixedF prec x₂) |+
              " " +| (fixedF prec t₁) |+ " " +| (fixedF prec t₂) |+
              " " +| pprintNum prec y δy |+ " " +| pprintNum prec z δz |+ ""
  putStrLn $ "σa = " +| (fixedF 5 σa) |+ ""
  putStrLn $ "a = " +| pprintNum 3 theA δa |+ ""
  putStrLn $ "ε = " +| (fixedF 3 (δa / theA * 100)) |+ "%"
