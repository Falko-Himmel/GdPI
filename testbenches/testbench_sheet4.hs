{-# LANGUAGE TupleSections #-}
module Main where

import System.Exit (exitFailure, exitSuccess)
import Text.Printf (printf)
import Data.List (intercalate)
import Prelude

-- Add file to test here:
-- ===============================================================================



-- ===============================================================================

-- Run testbench with:
-- ghc test_bench_name.hs
-- runhaskell test_bench_name.hs

-- helpers for testing
eps :: Double
eps = 1e-9

approxEq :: Double -> Double -> Bool
approxEq a b = abs (a - b) <= eps || (isNaN a && isNaN b)

-- Small pretty printers used in failure messages
showDrone :: Drone -> String
showDrone (ReplicationUnit r) = "ReplicationUnit " ++ show r
showDrone Egg = "Egg"
showDrone (Prototype r) = "Prototype " ++ show r
showDrone DevelopedDrone = "DevelopedDrone"

showHive :: Hive -> String
showHive = show

-- Test result datatype
data TestResult = Pass String | Fail String String String
-- Fail testname expected got

-- Run helpers
runTest :: String -> Bool -> TestResult
runTest name True  = Pass name
runTest name False = Fail name "" ""  -- caller will fill expected/got if needed

runAll :: [IO TestResult] -> IO [TestResult]
runAll = sequence

printResultFormatted :: TestResult -> IO ()
printResultFormatted (Pass name) = putStrLn $ "OK:  " ++ name
printResultFormatted (Fail name _ _) =
  case lookup name expectedGotPairs of
    Just (e,g) -> do
      putStrLn $ "FAIL: " ++ name
      putStrLn $ "  expected: " ++ e
      putStrLn $ "  got:      " ++ g
    Nothing -> putStrLn $ "FAIL: " ++ name

normalizeResult :: TestResult -> TestResult
normalizeResult p@(Pass _) = p
normalizeResult f@(Fail name _ _) =
  case lookup name expectedGotPairs of
    Just (e,g) -> Fail name e g
    Nothing -> f

-- Expected/got pairs for nicer output
expectedGotPairs :: [(String,(String,String))]
expectedGotPairs =
  [ ("calculateEnergyUsage sample == 22", ("22", show $ calculateEnergyUsage [Sensor 2, CommUnit, Drive 3, AnalysisUnit]))
  , ("calculateEnergyUsage [] == 0", ("0", show $ calculateEnergyUsage []))
  , ("develop (ReplicationUnit 0.5) -> ReplicationUnit 0.8", ("[ReplicationUnit 0.8]", show (develop (ReplicationUnit 0.5)))
    )
  , ("develop (ReplicationUnit 1.2) -> [ReplicationUnit 0, ReplicationUnit 0, Egg]", ("[ReplicationUnit 0, ReplicationUnit 0, Egg]", show (develop (ReplicationUnit 1.2))))
  , ("develop [Egg, DevelopedDrone] -> [[Prototype 0],[DevelopedDrone]]", ("[[Prototype 0],[DevelopedDrone]]", show (develop [Egg, DevelopedDrone])))
  , ("simulateDevelopment [Egg] !! 1 == [Prototype 0]", ("[Prototype 0]", show ((simulateDevelopment [Egg]) !! 1)))
  , ("accumulateOverTime counts DevelopedDrone over 3 steps == 1", ("1", show (accumulateOverTime (length . filter (== DevelopedDrone)) (+) 3 [Prototype 0.9])))
  ]

-- Tests for Sheet4

tests :: [IO TestResult]
tests =
  [ pure $ if calculateEnergyUsage [Sensor 2, CommUnit, Drive 3, AnalysisUnit] == 22
           then Pass "calculateEnergyUsage sample == 22"
           else Fail "calculateEnergyUsage sample == 22" "22" (show $ calculateEnergyUsage [Sensor 2, CommUnit, Drive 3, AnalysisUnit])

  , pure $ if calculateEnergyUsage [] == 0
           then Pass "calculateEnergyUsage [] == 0"
           else Fail "calculateEnergyUsage [] == 0" "0" (show $ calculateEnergyUsage [])

  , pure $ let got = develop (ReplicationUnit 0.5)
           in case got of
                [ReplicationUnit r] | approxEq r 0.8 -> Pass "develop (ReplicationUnit 0.5) -> ReplicationUnit 0.8"
                _ -> Fail "develop (ReplicationUnit 0.5) -> ReplicationUnit 0.8" "[ReplicationUnit 0.8]" (show got)

  , pure $ let got = develop (ReplicationUnit 1.2)
           in if got == [ReplicationUnit 0, ReplicationUnit 0, Egg]
              then Pass "develop (ReplicationUnit 1.2) -> [ReplicationUnit 0, ReplicationUnit 0, Egg]"
              else Fail "develop (ReplicationUnit 1.2) -> [ReplicationUnit 0, ReplicationUnit 0, Egg]" "[ReplicationUnit 0, ReplicationUnit 0, Egg]" (show got)

  , pure $ let got = develop [Egg, DevelopedDrone]
           in if got == [[Prototype 0], [DevelopedDrone]]
              then Pass "develop [Egg, DevelopedDrone] -> [[Prototype 0],[DevelopedDrone]]"
              else Fail "develop [Egg, DevelopedDrone] -> [[Prototype 0],[DevelopedDrone]]" "[[Prototype 0],[DevelopedDrone]]" (show got)

  , pure $ let got = (simulateDevelopment [Egg]) !! 1
           in if got == [Prototype 0]
              then Pass "simulateDevelopment [Egg] !! 1 == [Prototype 0]"
              else Fail "simulateDevelopment [Egg] !! 1 == [Prototype 0]" "[Prototype 0]" (show got)

  , pure $ let got = accumulateOverTime (length . filter (== DevelopedDrone)) (+) 3 [Prototype 0.9]
           in if got == 1
              then Pass "accumulateOverTime counts DevelopedDrone over 3 steps == 1"
              else Fail "accumulateOverTime counts DevelopedDrone over 3 steps == 1" "1" (show got)
  ]

main :: IO ()
main = do
  results <- runAll tests
  mapM_ printResultFormatted results
  let failed = [t | t@(Fail _ _ _) <- map normalizeResult results]
  putStrLn ""
  if null failed
    then putStrLn "All tests passed." >> exitSuccess
    else do
      putStrLn $ show (length failed) ++ " test(s) failed."
      -- Print failures detail (already printed above), exit failure
      exitFailure
report :: TestResult -> IO ()
report (Pass name) = putStrLn $ "OK:  " ++ name
report (Fail name expc got) = do
    putStrLn $ "FAIL: " ++ name
    putStrLn $ "  expected: " ++ expc
    putStrLn $ "  got:      " ++ got
