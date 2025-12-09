{-# LANGUAGE TupleSections #-}
module Main where

import System.Exit (exitFailure, exitSuccess)
import Text.Printf (printf)
import Data.List (intercalate)
import Prelude

-- Add file to test here:




--

-- Testing helper functions
eps :: Double
eps = 1e-9

approxEq :: Double -> Double -> Bool
approxEq a b = abs (a - b) <= eps || (isNaN a && isNaN b)

approxVecEq :: Vector -> Vector -> Bool
approxVecEq (x1,y1) (x2,y2) = approxEq x1 x2 && approxEq y1 y2

approxMatEq :: Matrix -> Matrix -> Bool
approxMatEq (a1,a2) (b1,b2) = approxVecEq a1 b1 && approxVecEq a2 b2

showVec :: Vector -> String
showVec (x,y) = printf "(%.6g, %.6g)" x y

showMat :: Matrix -> String
showMat (a,b) = printf "(%s, %s)" (showVec a) (showVec b)

data TestResult = Pass String | Fail String String String
-- Fail testname expected got

runTest :: String -> Bool -> TestResult
runTest name True  = Pass name
runTest name False = Fail name "" ""  -- caller will fill expected/got if needed

runAll :: [IO TestResult] -> IO [TestResult]
runAll = sequence

report :: TestResult -> IO ()
report (Pass name) = putStrLn $ "OK:  " ++ name
report (Fail name expc got) = do
    putStrLn $ "FAIL: " ++ name
    putStrLn $ "  expected: " ++ expc
    putStrLn $ "  got:      " ++ got

-- Actual test cases
tests :: [IO TestResult]
tests =
  [ pure $ if approxEq (magnitude (3,4)) 5
           then Pass "magnitude (3,4) == 5"
           else Fail "magnitude (3,4) == 5" "5" (show $ magnitude (3,4))

  , pure $ if approxEq (inner (1,2) (3,4)) 11
           then Pass "inner (1,2) (3,4) == 11"
           else Fail "inner (1,2) (3,4) == 11" "11" (show $ inner (1,2) (3,4))

  , pure $ let expected = (4,6)
               got = add (1,2) (3,4)
           in if approxVecEq expected got
              then Pass "add (1,2) (3,4) == (4,6)"
              else Fail "add (1,2) (3,4) == (4,6)" (showVec expected) (showVec got)

  , pure $ let expected = (3.0, -4.0)
               got = scale 2 (1.5, -2)
           in if approxVecEq expected got
              then Pass "scale 2 (1.5,-2) == (3,-4)"
              else Fail "scale 2 (1.5,-2) == (3,-4)" (showVec expected) (showVec got)

  , pure $ let expected = (0.6, 0.8)  -- normalize (3,4)
               got = normalize (3,4)
           in if approxVecEq expected got
              then Pass "normalize (3,4) == (0.6,0.8)"
              else Fail "normalize (3,4) == (0.6,0.8)" (showVec expected) (showVec got)

  , pure $ let m = ((1,2),(3,4))
               expected = ((1,3),(2,4))
               got = transpose m
           in if approxMatEq expected got
              then Pass "transpose ((1,2),(3,4))"
              else Fail "transpose ((1,2),(3,4))" (showMat expected) (showMat got)

  , pure $ let m = example
               expected = ((7,10),(15,22))  -- example * example
               got = mult m m
           in if approxMatEq expected got
              then Pass "mult example example == ((7,10),(15,22))"
              else Fail "mult example example" (showMat expected) (showMat got)

  , pure $ let m = example
               v = (1,0)
               expected = (1,3)   -- transform example (1,0) = (1,3)
               got = transform m v
           in if approxVecEq expected got
              then Pass "transform example (1,0) == (1,3)"
              else Fail "transform example (1,0)" (showVec expected) (showVec got)

  , pure $ let r = (pi/2)
               expected = (0,1)
               got = transform (rotation r) (1,0)
           in if approxVecEq expected got
              then Pass "rotation (pi/2) rotates (1,0) to (0,1)"
              else Fail "rotation (pi/2) rotates (1,0) to (0,1)" (showVec expected) (showVec got)
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

-- Helpers to format results with expected/got when available
normalizeResult :: TestResult -> TestResult
normalizeResult p@(Pass _) = p
normalizeResult f@(Fail name _ _) =
  case lookup name expectedGotPairs of
    Just (e,g) -> Fail name e g
    Nothing -> f

expectedGotPairs :: [(String,(String,String))]
expectedGotPairs =
  [ ("magnitude (3,4) == 5", ("5", show (magnitude (3,4))))
  , ("inner (1,2) (3,4) == 11", ("11", show (inner (1,2) (3,4))))
  , ("add (1,2) (3,4) == (4,6)", (showVec (4,6), showVec (add (1,2) (3,4))))
  , ("scale 2 (1.5,-2) == (3,-4)", (showVec (3,-4), showVec (scale 2 (1.5,-2))))
  , ("normalize (3,4) == (0.6,0.8)", (showVec (0.6,0.8), showVec (normalize (3,4))))
  , ("transpose ((1,2),(3,4))", (showMat ((1,3),(2,4)), showMat (transpose ((1,2),(3,4)))))
  , ("mult example example == ((7,10),(15,22))", (showMat ((7,10),(15,22)), showMat (mult example example)))
  , ("transform example (1,0) == (1,3)", (showVec (1,3), showVec (transform example (1,0))))
  , ("rotation (pi/2) rotates (1,0) to (0,1)", (showVec (0,1), showVec (transform (rotation (pi/2)) (1,0))))
  ]

printResultFormatted :: TestResult -> IO ()
printResultFormatted (Pass name) = putStrLn $ "OK:  " ++ name
printResultFormatted (Fail name _ _) =
  case lookup name expectedGotPairs of
    Just (e,g) -> do
      putStrLn $ "FAIL: " ++ name
      putStrLn $ "  expected: " ++ e
      putStrLn $ "  got:      " ++ g
    Nothing -> putStrLn $ "FAIL: " ++ name