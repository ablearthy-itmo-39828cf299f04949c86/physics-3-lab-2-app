{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
module Task2 (main) where

import           Control.Monad (forM_)
import           Data.List     (zip4)
import           Fmt
import           GHC.Float     (int2Double)
import qualified Numeric.AD    as D

data Measurement
  = Measurement
  { h  :: !Double
  , h' :: !Double
  , ts :: ![(Double, Double)]
  }

ms :: [Measurement]
ms =
  [ Measurement (200 / 1000) (192 / 1000)
    [ (1.4, 4.6)
    , (1.4, 4.6)
    , (1.3, 4.5)
    , (1.4, 4.6)
    , (1.4, 4.6)
    ]
  , Measurement (210 / 1000) (193 / 1000)
    [ (1.0, 3.4)
    , (1.0, 3.4)
    , (1.0, 3.4)
    , (1.0, 3.4)
    , (1.0, 3.4)
    ]
  , Measurement (218 / 1000) (193 / 1000)
    [ (0.8, 2.7)
    , (0.8, 2.7)
    , (0.8, 2.7)
    , (0.8, 2.7)
    , (0.8, 2.7)
    ]
  , Measurement (227 / 1000) (194 / 1000)
    [ (0.7, 2.3)
    , (0.7, 2.4)
    , (0.7, 2.3)
    , (0.7, 2.2)
    , (0.7, 2.3)
    ]
  , Measurement (237 / 1000) (195 / 1000)
    [ (0.6, 2.0)
    , (0.6, 2.0)
    , (0.6, 2.0)
    , (0.6, 2.0)
    , (0.6, 2.0)
    ]
  ]

h₀ = 191 / 1000
h₀' = 193 / 1000

x = 0.22
x' = 1

x₁ = 0.15
x₂ = 1.1

sinα :: Measurement -> Double
sinα m = ((m.h - h₀) - (m.h' - h₀')) / (x' - x)

sinαs = map sinα ms

mean :: Fractional a => [a] -> a
mean xs = s / l
  where
    (s, l) = foldr (\x (s', l') -> (s' + x, l' + 1)) (0, 0) xs

σ :: [Double] -> Double
σ xs = sqrt $ s / l / (l - 1)
  where
    m = mean xs
    l = int2Double $ length xs
    s = sum $ [ (x - m)^2 | x <- xs ]

tαN = 2.13
δT = 2 / 3 * 0.1

processT :: [Double] -> (Double, Double)
processT ts = (m, sqrt (δt^2 + δT^2))
  where
    m = mean ts
    σt = σ ts
    δt = tαN * σt

as_ = do
  (Measurement _ _ ts) <- ms
  let mt₁ = mean $ map fst ts
  let mt₂ = mean $ map snd ts
  pure $ 2 * (x₂ - x₁) / (mt₂^2 - mt₁^2)

δas = do
  ((Measurement _ _ ts), ma) <- zip ms as_
  let δx = 5 / 1000
  let (mt₁, δt₁) = processT $ map fst ts
  let (mt₂, δt₂) = processT $ map snd ts
  pure $ ma * sqrt (
    (δx^2 + δx^2)/(x₂ - x₁)^2
    + 4 * ((mt₁ * δt₁)^2 + (mt₂ * δt₂)^2) / (mt₂^2 - mt₁^2)^2)


theB = (s₁ - s₂ * s₃ / cnt) / (s₄ - s₃^2 / cnt)
  where
    s₁ = sum $ zipWith (*) as_ sinαs
    s₂ = sum as_
    s₃ = sum sinαs
    s₄ = sum $ map (^2) sinαs
    cnt = int2Double $ length ms

theA = (sum as_ - theB * (sum sinαs)) / (int2Double (length ms))

theD = sum (map (^2) sinαs) - (sum sinαs)^2 / (int2Double $ length ms)

σg = sqrt $ s₁ / (theD * (cnt - 2))
  where
    cnt = int2Double $ length ms
    s₁ = sum $ zipWith (\a s -> (a - (theA + theB * s))^2) as_ sinαs

δg = 2 * σg

g_real = 9.8195

pprintNum :: Int -> Double -> Double -> String
pprintNum prec a δa = "" +| (fixedF prec a) |+ "±" +| (fixedF prec δa) |+ ""

main :: IO ()
main = do
  let prec = 2
  putStrLn "sinα t₁±Δt₁ t₂±Δt₂ ⟨a⟩±Δa"
  forM_ (zip4 ms sinαs as_ δas) $ \((Measurement _ _ ts), s, a, δa) -> do
    let (mt₁, δt₁) = processT $ map fst ts
    let (mt₂, δt₂) = processT $ map snd ts
    putStrLn $ "" +| (fixedF 4 s) |+ " " +| pprintNum prec mt₁ δt₁ |+
              " " +| pprintNum prec mt₂ δt₂ |+
              " " +| pprintNum prec a δa |+ ""
  putStrLn $ "A = " +| fixedF 4 theA |+ ""
  putStrLn $ "B = " +| fixedF 4 theB |+ ""
  putStrLn $ "σg = " +| fixedF 4 σg |+ ""
  putStrLn $ "Δg = " +| fixedF 4 δg |+ ""
  putStrLn $ "g = " +| pprintNum prec theB δg |+ ""
  putStrLn $ "ε = " +| fixedF prec (δg / theB * 100) |+ "%"
  putStrLn $ "|g_real - g| = " +| fixedF 4 (abs (g_real - theB)) |+ ""
