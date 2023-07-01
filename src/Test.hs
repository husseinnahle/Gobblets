module Test (testRules, testMinimax) where

import System.Directory (listDirectory)
import Text.Printf (printf)
import System.IO (hFlush, stdout)
import Data.List (sort, isSuffixOf)

import State (State)
import Rules (availableMoves, applyMoves)
import Move (Move, parseMoves)
import Minimax (minimaxScore)

-- | Représente un fichier de test, contenant une partie
data TestCase = TestCase
  { name :: String  -- ^ Le nom du test
  , moves :: [Move] -- ^ Les mouvements de la partie
  , available :: Int -- ^ Le nombre de mouvements possibles attendus
  , score :: Int -- ^ Le score du minimax attendu
  }

-- | Construit un cas de test à partir d'un fichier nommé 'name', contenant les
-- lignes données en second argument.
testCase :: String -> [String] -> TestCase
testCase name (available : score : moves) = TestCase
  { name = name
  , available = read available
  , score = read score
  , moves = parseMoves moves
  }
testCase name _ = error $ "Invalid test case: " ++ name

-- | Charge toute la suite de test
tests :: IO [TestCase]
tests = do
  names <- listDirectory "tests/"
  -- mapM (\n -> testCase n . lines <$> readFile ("tests/" ++ n)) (sort names)
  mapM (\n -> testCase n . lines <$> readFile ("tests/" ++ n)) (sort (filter (not . isSuffixOf ".moves") names))

-- | Fonction d'aide pour lancer les tests
runTests :: (TestCase -> State -> IO ()) -> IO ()
runTests test =
  tests >>= mapM_ (\t -> case applyMoves $ moves t of
                        Just state -> test t state
                        Nothing -> printf "Test failed: cannot construct state from moves for %s" (name t))

-- | Exécute les tests de règles.
-- Peut être exécuté avec `stack run test-rules`.
testRules :: IO ()
testRules =
  runTests $ \test state ->
    printf "Testing %s... " (name test) >> hFlush stdout >>
    if length (availableMoves state) == available test
    then printf "OK!\n"
    else printf "failed: incorrect number of available moves for, got %d but expected %d\n" (length (availableMoves state)) (available test)

-- | La profondeur par défaut du minimax, telle qu'utilisée dans les tests
depth :: Int
depth = 5

-- | Exécute les tests de minimax
-- Peut être exécuté avec `stack run test-minimax`
testMinimax :: IO ()
testMinimax =
  runTests $ \test state ->
    printf "Testing %s... " (name test) >> hFlush stdout >>
    if minimaxScore state depth == score test
    then printf "OK!\n"
    else printf "failed: incorrect minimax score for, got %d but expected %d\n" (minimaxScore state depth) (score test)
